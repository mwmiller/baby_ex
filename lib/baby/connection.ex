defmodule Baby.Connection do
  @behaviour :gen_statem
  @behaviour :ranch_protocol
  require Logger
  alias Baby.Protocol

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
      pkt: [],
      wire: <<>>
    }
  end

  # Generic TCP handling stuff. Non-state dependant
  @impl true
  def handle_event({:timeout, :idle}, :nothing_happening, _, conn_info), do: disconnect(conn_info)

  def handle_event(:info, {:tcp_closed, _socket}, _, conn_info), do: disconnect(conn_info)
  def handle_event(:info, {:tcp, _socket, data}, _, conn_info), do: wire_buffer(data, conn_info)

  def handle_event(:enter, :hello, :hello, conn_info) do
    {:keep_state, Protocol.outbound(conn_info, :HELLO), []}
  end

  def handle_event(:enter, :hello, :auth, conn_info) do
    {:keep_state, Protocol.outbound(conn_info, :AUTH)}
  end

  def handle_event(:enter, :auth, :replicate, conn_info) do
    {:keep_state, Protocol.outbound(conn_info, :HAVE), []}
  end

  def handle_event(:internal, :data, :hello, %{pkt: [{1, hello} | rest]} = conn_info) do
    case Protocol.inbound(hello, conn_info, :HELLO) do
      :error ->
        disconnect(conn_info)

      nci ->
        log_traffic(nci, :in, :HELLO)
        {:next_state, :auth, %{nci | pkt: rest}, []}
    end
  end

  def handle_event(:internal, :data, :auth, %{pkt: [{2, _} = nonce_box | rest]} = conn_info) do
    log_traffic(conn_info, :in, :AUTH)

    case Protocol.inbound(nonce_box, conn_info, :AUTH) do
      :error ->
        disconnect(conn_info)

      nci ->
        Logger.info([nci.short_peer, " connected"])
        {:next_state, :replicate, %{nci | pkt: rest}, [@idle_timeout]}
    end
  end

  def handle_event(:internal, :data, :replicate, %{pkt: [{5, _} = nonce_box | rest]} = conn_info) do
    log_traffic(conn_info, :in, :HAVE)

    case Protocol.inbound(nonce_box, conn_info, :HAVE) do
      :error ->
        disconnect(conn_info)

      nci ->
        {:keep_state, %{nci | pkt: rest}, [@idle_timeout, {:next_event, :internal, :data}]}
    end
  end

  def handle_event(:internal, :data, :replicate, %{pkt: [{6, _} = nonce_box | rest]} = conn_info) do
    log_traffic(conn_info, :in, :WANT)

    case Protocol.inbound(nonce_box, conn_info, :WANT) do
      :error ->
        disconnect(conn_info)

      nci ->
        {:keep_state, %{nci | pkt: rest}, [@idle_timeout, {:next_event, :internal, :data}]}
    end
  end

  def handle_event(:internal, :data, :replicate, %{pkt: [{8, _} = nonce_box | rest]} = conn_info) do
    log_traffic(conn_info, :in, :BAMB)

    case Protocol.inbound(nonce_box, conn_info, :BAMB) do
      :error ->
        disconnect(conn_info)

      nci ->
        {:keep_state, %{nci | pkt: rest}, [@idle_timeout, {:next_event, :internal, :data}]}
    end
  end

  def handle_event(:internal, :data, state, %{pkt: [{type, _} | _]} = conn_info) do
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

  def handle_event(:internal, :data, _, conn_info) do
    {:keep_state, conn_info, [@idle_timeout]}
  end

  defp wire_buffer(data, conn_info) do
    active_once(conn_info)
    wire = conn_info.wire <> data

    case Stlv.decode(wire) do
      :error ->
        {:keep_state, %{conn_info | :wire => wire},
         [@idle_timeout, {:next_event, :internal, :data}]}

      {type, value, rest} ->
        wire_buffer(rest, %{conn_info | pkt: conn_info.pkt ++ [{type, value}], wire: <<>>})

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

  def send_packet(packet, ci, type) do
    log_traffic(ci, :out, type)
    send_packet(packet, ci)
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
