defmodule Baby.Connection do
  @behaviour :gen_statem
  @behaviour :ranch_protocol
  require Logger

  @impl true
  def callback_mode(), do: :handle_event_function

  @impl true
  def start_link(ref, transport, identity) do
    {:ok, :proc_lib.spawn_link(__MODULE__, :init, [{ref, transport, identity}])}
  end

  def start_link(opts) do
    :gen_statem.start_link(__MODULE__, opts, [])
  end

  @impl true
  def init({ref, transport, identity}) do
    {:ok, socket} = :ranch.handshake(ref)
    :ok = transport.setopts(socket, active: :once)

    Logger.debug("RANCH init")

    :gen_statem.enter_loop(
      __MODULE__,
      [],
      :connected,
      initial_conn_info(identity, socket, transport),
      [
        {:next_event, :internal, :say_hello}
      ]
    )
  end

  def init(opts) do
    identity = Keyword.get(opts, :identity)

    {:ok, socket} =
      :gen_tcp.connect(Keyword.get(opts, :host), Keyword.get(opts, :port), [
        :binary,
        active: :once
      ])

    Logger.debug("SOCKET init")

    {:ok, :connected, initial_conn_info(identity, socket, nil),
     [{:next_event, :internal, :say_hello}]}
  end

  def initial_conn_info(identity, socket, transport) do
    %{
      clump_id: "Quagga",
      socket: socket,
      transport: transport,
      our_pk: Baobab.identity_key(identity, :public),
      our_sk: Baobab.identity_key(identity, :secret),
      pkt: nil,
      wire: <<>>
    }
  end

  # Generic TCP handling stuff. Non-state dependant
  @impl true
  def handle_event(:info, {:tcp_closed, _socket}, _, conn_info), do: disconnect(conn_info)

  def handle_event(:info, {:tcp, _socket, data}, _, conn_info), do: wire_buffer(data, conn_info)

  def handle_event(:internal, :data, :connected, conn_info) do
    # We don't really handle any data in this state, so move it on to hello
    {:next_state, :hello, conn_info, [{:next_event, :internal, :data}]}
  end

  def handle_event(:internal, :say_hello, _, conn_info) do
    {esk, epk} = Kcl.generate_key_pair(:encrypt)
    Logger.debug("Said hello")

    (conn_info.our_pk <> epk <> Kcl.auth(epk, conn_info.clump_id))
    |> Stlv.encode(1)
    |> send_packet(conn_info)

    {:next_state, :hello, Map.merge(conn_info, %{our_epk: epk, our_esk: esk}), []}
  end

  def handle_event(:internal, :data, :hello, %{pkt: {1, hello}} = conn_info) do
    case hello do
      <<their_pk::binary-size(32), their_epk::binary-size(32), hmac::binary-size(32)>> ->
        case Kcl.valid_auth?(hmac, their_epk, conn_info.clump_id) do
          false ->
            disconnect(conn_info)

          true ->
            Logger.debug("Accepted hello")

            {:next_state, :auth,
             Map.merge(conn_info, %{their_pk: their_pk, their_epk: their_epk}),
             [{:next_event, :internal, :send_auth}]}
        end

      _ ->
        disconnect(conn_info)
    end
  end

  def handle_event(:internal, :send_auth, :auth, conn_info) do
    send_key =
      Curve25519.derive_shared_secret(
        conn_info.our_esk,
        Kcl.sign_to_encrypt(conn_info.their_pk, :public)
      )
      |> Blake2.hash2b(32)

    recv_key =
      Curve25519.derive_shared_secret(
        Kcl.sign_to_encrypt(conn_info.our_sk, :secret),
        conn_info.their_epk
      )
      |> Blake2.hash2b(32)

    nci = Map.merge(conn_info, %{:recv_key => recv_key, :send_key => send_key})

    (conn_info.clump_id <> recv_key)
    |> Kcl.sign(conn_info.our_sk)
    |> pack_and_ship_nonce_box(nci, 2)

    Logger.debug("Sent auth")

    {:keep_state, nci}
  end

  def handle_event(:internal, :data, :auth, %{pkt: {2, _}} = conn_info) do
    sig = unpack_nonce_box(conn_info)
    whosit = "~" <> (conn_info.their_pk |> Baobab.b62identity() |> String.slice(0..6))

    case Kcl.valid_signature?(
           sig,
           conn_info.clump_id <> conn_info.send_key,
           conn_info.their_pk
         ) do
      false ->
        Logger.debug(["Auth denied: ", whosit])
        disconnect(conn_info)

      true ->
        Logger.info(["Connected to: ", whosit])

        {:next_state, :replicate,
         Map.drop(conn_info, [
           :our_pk,
           :our_sk,
           :our_esk,
           :our_epk,
           :their_pk,
           :their_epk
         ])
         |> Map.merge(%{peer: whosit}), [{:next_event, :internal, :send_have}]}
    end
  end

  def handle_event(:internal, :send_have, :replicate, conn_info) do
    Baobab.stored_info()
    |> CBOR.encode()
    |> Stlv.encode(1)
    |> pack_and_ship_nonce_box(conn_info, 3)

    Logger.debug("HAVE sent")

    {:keep_state_and_data, []}
  end

  def handle_event(:info, {:want, wants}, :replicate, conn_info) do
    wants
    |> CBOR.encode()
    |> Stlv.encode(2)
    |> pack_and_ship_nonce_box(conn_info, 3)

    Logger.debug("WANT sent")
    {:keep_state_and_data, []}
  end

  def handle_event(:internal, :data, :replicate, %{pkt: {3, _}} = conn_info) do
    case conn_info |> unpack_nonce_box |> Stlv.decode() do
      {1, data, ""} ->
        case CBOR.decode(data) do
          {:ok, decoded, ""} -> Process.send(self(), {:want, decoded}, [])
          _ -> disconnect(conn_info)
        end

      {2, data, ""} ->
        case CBOR.decode(data) do
          {:ok, decoded, ""} -> send_wants(decoded, conn_info)
          _ -> disconnect(conn_info)
        end

      {8, data, ""} ->
        case CBOR.decode(data) do
          {:ok, decoded, ""} ->
            Logger.debug(["Received bamboo list: ", length(decoded) |> Integer.to_string()])
            Baobab.import(decoded)

          _ ->
            disconnect(conn_info)
        end

      _ ->
        disconnect(conn_info)
    end

    {:keep_state, %{conn_info | pkt: nil}}
  end

  defp send_wants([], _), do: :ok

  defp send_wants([[a, l, e] | rest], conn_info) do
    a
    |> Baobab.log_at(e, log_id: l, format: :binary)
    |> CBOR.encode()
    |> Stlv.encode(8)
    |> pack_and_ship_nonce_box(conn_info, 3)

    send_wants(rest, conn_info)
  end

  defp pack_and_ship_nonce_box(msg, conn_info, type) do
    nonce = :rand.bytes(24)

    (nonce <> Kcl.secretbox(msg, nonce, conn_info.send_key))
    |> Stlv.encode(type)
    |> send_packet(conn_info)
  end

  def unpack_nonce_box(
        %{pkt: {_, <<nonce::binary-size(24), box::binary>>}, recv_key: recv_key} = conn_info
      ) do
    case Kcl.secretunbox(box, nonce, recv_key) do
      :error ->
        Logger.debug("Unboxing error")
        disconnect(conn_info)

      msg ->
        msg
    end
  end

  defp wire_buffer(data, conn_info) do
    active_once(conn_info)
    wire = conn_info.wire <> data

    case Stlv.decode(wire) do
      :error ->
        {:keep_state, %{conn_info | :wire => wire}, []}

      {type, value, rest} ->
        {:keep_state, %{conn_info | pkt: {type, value}, wire: rest},
         [{:next_event, :internal, :data}]}

      _ ->
        disconnect(conn_info)
    end
  end

  defp disconnect(conn_info) do
    case Map.fetch(conn_info, :peer) do
      {:ok, peer} -> Logger.info(["Connection closing: ", peer])
      :error -> :ok
    end

    close_connection(conn_info)
    {:next_state, :disconnected, %{}, []}
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
