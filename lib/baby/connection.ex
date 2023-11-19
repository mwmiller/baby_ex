defmodule Baby.Connection do
  @behaviour :gen_statem
  @behaviour :ranch_protocol
  alias Baby.{Protocol, Util}
  alias Baby.Connection.Registry

  @moduledoc """
  State machine connection handler
  """

  @impl true
  def callback_mode(), do: [:handle_event_function, :state_enter]

  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      type: :worker,
      restart: :temporary
    }
  end

  @impl true
  def start_link(ref, _, transport, opts) do
    {:ok, :proc_lib.spawn_link(__MODULE__, :init, [{ref, transport, opts}])}
  end

  def start_link(opts) do
    :gen_statem.start_link(__MODULE__, opts, [])
  end

  @impl true
  def init({ref, transport, opts}) do
    {:ok, socket} = :ranch.handshake(ref)
    {:ok, peer} = transport.peername(socket)
    :ok = transport.setopts(socket, active: :once)
    # For now, we aren't going to prevent the same host
    # from connecting more than once.
    Registry.register_name(peer, self())

    :gen_statem.enter_loop(__MODULE__, [], :hello, initial_conn_info(opts, socket, transport), [])
  end

  def init(opts) do
    host = Keyword.get(opts, :host)
    port = Keyword.get(opts, :port)
    key = {host, port}

    # For outbound we simply want to prevent connecting to them twice
    with false <- Registry.active?(key),
         {:ok, socket} <- :gen_tcp.connect(host, port, [:binary, active: :once]) do
      Registry.register_name({host, port}, self())
      {:ok, :hello, initial_conn_info(opts, socket, nil), []}
    else
      _ ->
        {:stop, :normal}
    end
  end

  @impl true
  def terminate(_, conn_info, _data) when is_map(conn_info), do: close_connection(conn_info)
  def terminate(_, _, _), do: :ok

  defp initial_conn_info(opts, socket, transport) do
    identity = Keyword.get(opts, :identity)
    clump_id = Keyword.get(opts, :clump_id, "Quagga")
    outrate = 100 |> Primacy.primes_near(count: 10, dir: :above) |> Enum.random()
    max_spins = 100 |> Primacy.primes_near(count: 5, dir: :below) |> Enum.random()

    Process.send_after(self(), :outbox, outrate, [])

    %{
      pid: self(),
      shoots: [],
      clump_id: clump_id,
      socket: socket,
      transport: transport,
      our_pk: Baobab.Identity.key(identity, :public),
      our_sk: Baobab.Identity.key(identity, :secret),
      their_nonces: MapSet.new(),
      inbox: [],
      outbox: [],
      outrate: outrate,
      wire: <<>>,
      spins: 0,
      max_spins: max_spins
    }
  end

  # Generic TCP handling stuff. Non-state dependant
  @impl true
  def handle_event(:info, {:tcp_closed, _socket}, _, conn_info), do: disconnect(conn_info)

  def handle_event(:info, {:tcp, _socket, data}, _, conn_info) do
    wire_buffer(data, conn_info)
  end

  def handle_event(:info, :outbox, _, %{spins: s, max_spins: ms} = conn_info) when s >= ms do
    disconnect(conn_info)
  end

  def handle_event(
        :info,
        :outbox,
        _,
        %{outbox: [{packet, type} | rest], outrate: rate, pid: pid} = conn_info
      ) do
    Util.connection_log(conn_info, :out, type)
    send_packet(packet, conn_info)
    Process.send_after(pid, :outbox, rate)
    {:keep_state, %{conn_info | outbox: rest}, []}
  end

  def handle_event(:info, :outbox, _, %{shoots: shoots, outrate: rate} = conn_info)
      when length(shoots) > 0 do
    # We have a non-empty shoots list
    # Yeah, this is unsatisfyingly written
    Process.send_after(conn_info.pid, :outbox, rate)
    {:keep_state, Protocol.outbound(%{conn_info | spins: 0}, :BAMB), []}
  end

  def handle_event(:info, :outbox, _, %{pid: pid, spins: s, outrate: rate} = conn_info) do
    Process.send_after(pid, :outbox, rate)
    {:keep_state, %{conn_info | spins: s + 1}, []}
  end

  def handle_event(:enter, :hello, :hello, conn_info) do
    {:keep_state, Protocol.outbound(conn_info, :HELLO), []}
  end

  def handle_event(:enter, :hello, :auth, conn_info) do
    {:keep_state, Protocol.outbound(conn_info, :AUTH), []}
  end

  def handle_event(:enter, :auth, :replicate, conn_info) do
    {:keep_state, Protocol.outbound(conn_info, :HAVE), []}
  end

  def handle_event(:info, {:added, clump_id, new}, :replicate, conn_info) do
    case clump_id == conn_info.clump_id do
      true -> {:keep_state, Protocol.outbound(Map.merge(conn_info, %{added: new}), :HAVE), []}
      false -> {:keep_state, conn_info, []}
    end
  end

  # Another connection might have stuff we cannot use while not replicating
  def handle_event(:info, {:added, _, _}, _, conn_info), do: {:keep_state, conn_info, []}

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
          Util.log_fatal(conn_info, "malformed packet")
          disconnect(conn_info)

        nci ->
          Util.connection_log(nci, :in, unquote(name))
          {:next_state, unquote(outstate), Map.merge(nci, %{inbox: rest, spins: 0}), []}
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

    Util.log_fatal(conn_info, "type " <> stype <> " message in state " <> Atom.to_string(state))
    disconnect(conn_info)
  end

  def handle_event(:info, :inbox, _, %{spins: s, max_spins: ms} = conn_info) when s >= ms do
    disconnect(conn_info)
  end

  # We might be out of sync, so we'll just go around again
  def handle_event(:info, :inbox, _, %{pid: pid, spins: s} = conn_info) do
    Process.send(pid, :inbox, [])
    {:keep_state, %{conn_info | spins: s + 1}, []}
  end

  defp wire_buffer(data, %{pid: pid, inbox: inbox, wire: cw} = conn_info) do
    active_once(conn_info)
    wire = cw <> data

    cond do
      length(inbox) > 10 ->
        # Yield
        Process.send(pid, :inbox, [])
        {:keep_state, %{conn_info | :wire => wire}, []}

      true ->
        case Stlv.decode(wire) do
          :error ->
            {:keep_state, %{conn_info | :wire => wire}, []}

          {type, value, rest} ->
            Process.send(pid, :inbox, [])
            wire_buffer(rest, %{conn_info | inbox: inbox ++ [{type, value}], wire: <<>>})

          _ ->
            Util.log_fatal(conn_info, "unexpected STLV error")
            disconnect(conn_info)
        end
    end
  end

  defp disconnect(conn_info) do
    # We don't want to log health check connections
    case Map.has_key?(conn_info, :short_peer) do
      false -> :ok
      true -> Util.connection_log(conn_info, :both, "disconnected", :info)
    end

    {:stop, :normal}
  end

  defp send_packet(packet, %{:transport => nil, :socket => sock}), do: :gen_tcp.send(sock, packet)
  defp send_packet(packet, %{:transport => trans, :socket => sock}), do: trans.send(sock, packet)

  defp active_once(%{:transport => nil, :socket => socket}),
    do: :inet.setopts(socket, active: :once)

  defp active_once(%{:transport => transport, :socket => socket}),
    do: transport.setopts(socket, active: :once)

  defp close_connection(%{:transport => nil, :socket => socket}), do: :gen_tcp.close(socket)

  defp close_connection(%{:transport => transport, :socket => socket}),
    do: transport.close(socket)
end
