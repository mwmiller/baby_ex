defmodule Baby.Connection do
  @behaviour :gen_statem
  @behaviour :ranch_protocol
  require Logger
  alias Baby.Protocol

  @inrate 101
  @outrate 103
  @idle_timeout {{:timeout, :idle}, 27901, :nothing_happening}
  @impl true
  def callback_mode(), do: [:handle_event_function, :state_enter]

  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      type: :worker,
      restart: :transient
    }
  end

  @impl true
  def start_link(ref, transport, opts) do
    {:ok, :proc_lib.spawn_link(__MODULE__, :init, [{ref, transport, opts}])}
  end

  def start_link(opts) do
    :gen_statem.start_link(__MODULE__, opts, [])
  end

  @impl true
  def init({ref, transport, opts}) do
    {:ok, socket} = :ranch.handshake(ref)
    :ok = transport.setopts(socket, active: :once)

    :gen_statem.enter_loop(
      __MODULE__,
      [],
      :hello,
      initial_conn_info(opts, socket, transport),
      [
        @idle_timeout
      ]
    )
  end

  def init(opts) do
    case :gen_tcp.connect(Keyword.get(opts, :host), Keyword.get(opts, :port), [
           :binary,
           active: :once
         ]) do
      {:ok, socket} ->
        {:ok, :hello, initial_conn_info(opts, socket, nil), [@idle_timeout]}

      _ ->
        {:stop, :normal}
    end
  end

  @impl true
  def terminate(_, conn_info, _data) when is_map(conn_info) do
    Logger.debug("Terminating")
    close_connection(conn_info)
  end

  def terminate(_, _, _), do: :ok

  def initial_conn_info(opts, socket, transport) do
    identity = Keyword.get(opts, :identity)
    Process.send_after(self(), :outbox, @outrate, [])
    Process.send_after(self(), :inbox, @inrate, [])

    %{
      pid: self(),
      have: stored_info_map(),
      want: %{},
      shoots: [],
      clump_id: Keyword.get(opts, :clump_id, "Quagga"),
      socket: socket,
      transport: transport,
      our_pk: Baobab.identity_key(identity, :public),
      our_sk: Baobab.identity_key(identity, :secret),
      their_nonces: MapSet.new(),
      inbox: [],
      outbox: [],
      wire: <<>>
    }
  end

  # Generic TCP handling stuff. Non-state dependant
  @impl true
  def handle_event({:timeout, :idle}, :nothing_happening, _, conn_info), do: disconnect(conn_info)

  def handle_event(:info, {:tcp_closed, _socket}, _, conn_info), do: disconnect(conn_info)
  def handle_event(:info, {:tcp, _socket, data}, _, conn_info), do: wire_buffer(data, conn_info)

  def handle_event(:info, :outbox, _, %{outbox: [{packet, type} | rest]} = conn_info) do
    log_traffic(conn_info, :out, type)
    send_packet(packet, conn_info)
    Process.send_after(conn_info.pid, :outbox, @outrate)
    {:keep_state, %{conn_info | outbox: rest}, [@idle_timeout]}
  end

  def handle_event(:info, :outbox, _, %{outbox: []} = conn_info) do
    Process.send_after(conn_info.pid, :outbox, @outrate)
    {:keep_state, conn_info, []}
  end

  def handle_event(:enter, :hello, :hello, conn_info) do
    {:keep_state, Protocol.outbound(conn_info, :HELLO), []}
  end

  def handle_event(:enter, :hello, :auth, conn_info) do
    {:keep_state, Protocol.outbound(conn_info, :AUTH)}
  end

  def handle_event(:enter, :auth, :replicate, conn_info) do
    {:keep_state, Protocol.outbound(conn_info, :HAVE), []}
  end

  # Write out the proto handlers
  for {name, %{type: type, instate: instate, outstate: outstate}} <-
        Protocol.definition() |> Map.to_list() do
    def handle_event(
          :info,
          :inbox,
          unquote(instate),
          %{inbox: [{unquote(type), _} = packet | rest]} = conn_info
        ) do
      case Protocol.inbound(packet, conn_info, unquote(name)) do
        :error ->
          disconnect(conn_info)

        nci ->
          log_traffic(nci, :in, unquote(name))
          Process.send_after(nci.pid, :inbox, @inrate, [])
          {:next_state, unquote(outstate), %{nci | inbox: rest}, []}
      end
    end
  end

  # Not defined in the protocol
  def handle_event(:info, :inbox, state, %{inbox: [{type, _} | _]} = conn_info) do
    stype =
      case Protocol.msglookup(type) do
        nil -> Integer.to_string(type)
        a -> Atom.to_string(a)
      end

    Logger.warn(
      tilde_peer(conn_info) <>
        " sent type " <> stype <> " message in state " <> Atom.to_string(state)
    )

    disconnect(conn_info)
  end

  # We might be out of sync, so we'll just go around again
  def handle_event(:info, :inbox, _, conn_info) do
    Process.send_after(conn_info.pid, :inbox, @inrate, [])
    {:keep_state, conn_info, []}
  end

  defp wire_buffer(data, conn_info) do
    active_once(conn_info)
    wire = conn_info.wire <> data

    case Stlv.decode(wire) do
      :error ->
        {:keep_state, %{conn_info | :wire => wire}, [@idle_timeout]}

      {type, value, rest} ->
        wire_buffer(rest, %{conn_info | inbox: conn_info.inbox ++ [{type, value}], wire: <<>>})

      _ ->
        disconnect(conn_info)
    end
  end

  defp disconnect(conn_info) do
    case tilde_peer(conn_info) do
      "~unknown" ->
        :ok

      dude ->
        Logger.info([dude <> " disconnected"])
    end

    {:stop, :normal}
  end

  def send_packet(packet, %{:transport => nil, :socket => sock}), do: :gen_tcp.send(sock, packet)
  def send_packet(packet, %{:transport => trans, :socket => sock}), do: trans.send(sock, packet)

  defp active_once(%{:transport => nil, :socket => socket}),
    do: :inet.setopts(socket, active: :once)

  defp active_once(%{:transport => transport, :socket => socket}),
    do: transport.setopts(socket, active: :once)

  defp close_connection(%{:transport => nil, :socket => socket}), do: :gen_tcp.close(socket)

  defp close_connection(%{:transport => transport, :socket => socket}),
    do: transport.close(socket)

  def arrow(:in), do: "→"
  def arrow(:out), do: "←"

  defp log_traffic(conn_info, dir, type) do
    Enum.join([tilde_peer(conn_info), arrow(dir), Atom.to_string(type)], " ")
    |> Logger.debug()
  end

  def tilde_peer(conn_info) do
    case Map.fetch(conn_info, :short_peer) do
      {:ok, them} -> them
      :error -> "~unknown"
    end
  end

  defp stored_info_map() do
    Baobab.stored_info() |> Enum.reduce(%{}, fn {a, l, e}, acc -> Map.put(acc, {a, l}, e) end)
  end
end
