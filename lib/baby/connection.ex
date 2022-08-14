defmodule Baby.Connection do
  @behaviour :gen_statem
  @behaviour :ranch_protocol
  require Logger

  @protodef %{0 => :BYE, 1 => :HELLO, 2 => :AUTH, 3 => :REPLICATE}
  @proto_msg Map.merge(
               @protodef,
               @protodef |> Map.to_list() |> Map.new(fn {k, v} -> {v, k} end)
             )

  @repdef %{1 => :HAVE, 2 => :WANT, 8 => :BAMB}
  @replication_msg Map.merge(
                     @repdef,
                     @repdef |> Map.to_list() |> Map.new(fn {k, v} -> {v, k} end)
                   )
  @impl true
  def callback_mode(), do: :handle_event_function

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
      :connected,
      initial_conn_info(opts, socket, transport),
      [
        {:next_event, :internal, :say_hello}
      ]
    )
  end

  def init(opts) do
    {:ok, socket} =
      :gen_tcp.connect(Keyword.get(opts, :host), Keyword.get(opts, :port), [
        :binary,
        active: :once
      ])

    {:ok, :connected, initial_conn_info(opts, socket, nil),
     [{:next_event, :internal, :say_hello}]}
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
      want: MapSet.new(),
      clump_id: Keyword.get(opts, :clump_id, "Quagga"),
      socket: socket,
      transport: transport,
      our_pk: Baobab.identity_key(identity, :public),
      our_sk: Baobab.identity_key(identity, :secret),
      pkt: [],
      wire: <<>>
    }
  end

  # Generic TCP handling stuff. Non-state dependant
  @impl true
  def handle_event(:info, {:tcp_closed, _socket}, _, conn_info) do
    disconnect(conn_info)
  end

  def handle_event(:info, {:tcp, _socket, data}, _, conn_info), do: wire_buffer(data, conn_info)

  def handle_event(:internal, :data, :connected, conn_info) do
    # We don't really handle any data in this state, so move it on to hello
    {:next_state, :hello, conn_info, [{:next_event, :internal, :data}]}
  end

  def handle_event(:internal, :say_hello, _, conn_info) do
    {esk, epk} = Kcl.generate_key_pair(:encrypt)
    type = :HELLO

    (conn_info.our_pk <> epk <> Kcl.auth(epk, conn_info.clump_id))
    |> Stlv.encode(@proto_msg[type])
    |> send_packet(conn_info)

    log_traffic(conn_info, :out, type)

    {:next_state, :hello, Map.merge(conn_info, %{our_epk: epk, our_esk: esk}), []}
  end

  def handle_event(:internal, :data, :hello, %{pkt: [{1, hello} | rest]} = conn_info) do
    case hello do
      <<their_pk::binary-size(32), their_epk::binary-size(32), hmac::binary-size(32)>> ->
        peer = their_pk |> Baobab.b62identity()
        short_peer = "~" <> (peer |> String.slice(0..6))

        case Kcl.valid_auth?(hmac, their_epk, conn_info.clump_id) do
          false ->
            disconnect(conn_info)

          true ->
            nci =
              Map.merge(conn_info, %{
                short_peer: short_peer,
                peer: peer,
                their_pk: their_pk,
                their_epk: their_epk
              })

            log_traffic(nci, :in, :HELLO)

            {:next_state, :auth, %{nci | pkt: rest}, [{:next_event, :internal, :send_auth}]}
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

    type = :AUTH

    (conn_info.clump_id <> recv_key)
    |> Kcl.sign(conn_info.our_sk)
    |> pack_and_ship_nonce_box(nci, type)

    log_traffic(conn_info, :out, type)
    {:keep_state, nci}
  end

  def handle_event(:internal, :data, :auth, %{pkt: [{2, _} | rest]} = conn_info) do
    sig = unpack_nonce_box(conn_info)
    log_traffic(conn_info, :in, :AUTH)

    case Kcl.valid_signature?(
           sig,
           conn_info.clump_id <> conn_info.send_key,
           conn_info.their_pk
         ) do
      false ->
        disconnect(conn_info)

      true ->
        Logger.info([conn_info.short_peer, " connected"])

        {:next_state, :replicate,
         Map.drop(conn_info, [
           :our_pk,
           :our_sk,
           :our_esk,
           :our_epk,
           :their_pk,
           :their_epk
         ])
         |> Map.merge(%{pkt: rest}), [{:next_event, :internal, :send_have}]}
    end
  end

  def handle_event(:internal, :send_have, :replicate, conn_info) do
    nci =
      conn_info.have
      |> Map.to_list()
      |> Enum.map(fn {{a, l}, e} -> {a, l, e} end)
      |> encode_replication(:HAVE, conn_info)

    {:keep_state, nci, [{:next_event, :internal, :data}]}
  end

  def handle_event(:internal, :data, :replicate, %{pkt: [{3, _} | rest]} = conn_info) do
    nci =
      case conn_info |> unpack_nonce_box |> Stlv.decode() do
        {msg_type, cbor, ""} ->
          case CBOR.decode(cbor) do
            {:ok, decoded, ""} ->
              what = @replication_msg[msg_type]
              log_traffic(conn_info, :in, what, decoded)
              replication_action(decoded, conn_info, what)

            _ ->
              disconnect(conn_info)
          end

        _ ->
          disconnect(conn_info)
      end

    {:keep_state, %{nci | pkt: rest}, [{:next_event, :internal, :data}]}
  end

  def handle_event(:internal, :data, :replicate, conn_info), do: {:keep_state, conn_info}

  defp replication_action(data, conn_info, :HAVE), do: request_their(data, conn_info, [])
  defp replication_action(data, conn_info, :WANT), do: send_our(data, conn_info)
  defp replication_action(data, conn_info, :BAMB), do: import_their(data, conn_info)

  defp import_their(stuff, conn_info) do
    stuff |> Baobab.import() |> import_summary(conn_info)
  end

  defp import_summary([], conn_info), do: conn_info

  defp import_summary([{:error, reason} | rest], conn_info) do
    Enum.join([tilde_peer(conn_info), arrow(:in), "import error:", reason], " ")
    |> Logger.warn()

    import_summary(rest, conn_info)
  end

  # They have to be provided in order or the chain won't verify
  # There are extra updates here, but maybe there's an error mixed in
  defp import_summary([%Baobab.Entry{author: author, log_id: l, seqnum: e} | rest], conn_info) do
    a = author |> Baobab.b62identity()

    import_summary(rest, %{
      conn_info
      | have: Map.merge(conn_info.have, %{{a, l} => e}),
        want:
          Enum.reduce([{a}, {a, l}, {a, l, e}], conn_info.want, fn elem, acc ->
            MapSet.delete(acc, elem)
          end)
    })
  end

  defp request_their([], conn_info, wants) do
    # When talking directly to the source, get as much
    # as one can of their logs.
    full_wants = wants ++ [{conn_info.peer}]

    encode_replication(
      full_wants,
      :WANT,
      %{
        conn_info
        | want: Enum.reduce(full_wants, conn_info.want, fn e, ms -> MapSet.put(ms, e) end)
      }
    )
  end

  defp request_their([[a, l, e] | rest], conn_info, acc) do
    we_have = Map.get(conn_info.have, {a, l}, 0)

    add =
      cond do
        # It's new to us, get everything we can
        we_have == 0 -> [{a, l}]
        # catch up
        we_have < e -> [{a, l, e}]
        # We're even or ahead -- we assume they'll ask if they want more
        we_have >= e -> []
      end

    request_their(rest, conn_info, acc ++ add)
  end

  defp send_our([], conn_info), do: conn_info

  # Full logs for author
  defp send_our([[a] | rest], conn_info) do
    conn_info.have
    |> Map.keys()
    |> Enum.reduce([], fn entry, acc ->
      case entry do
        {^a, l} -> [[a, l] | acc]
        _ -> acc
      end
    end)
    |> then(fn al -> rest ++ al end)
    |> send_our(conn_info)
  end

  # Full log for author log_id
  defp send_our([[a, l] | rest], conn_info) do
    nci =
      case Baobab.full_log(a, log_id: l, format: :binary) do
        [] -> conn_info
        entries -> encode_replication(entries, :BAMB, conn_info)
      end

    send_our(rest, nci)
  end

  defp send_our([[a, l, e] | rest], conn_info) do
    nci =
      case Baobab.log_at(a, e, log_id: l, format: :binary) do
        [] -> conn_info
        entries -> encode_replication(entries, :BAMB, conn_info)
      end

    send_our(rest, nci)
  end

  defp encode_replication(msg, type, conn_info) do
    msg
    |> CBOR.encode()
    |> Stlv.encode(@replication_msg[type])
    |> pack_and_ship_nonce_box(conn_info, :REPLICATE)

    log_traffic(conn_info, :out, type, msg)
    conn_info
  end

  defp pack_and_ship_nonce_box(msg, conn_info, type) do
    nonce = :rand.bytes(24)

    (nonce <> Kcl.secretbox(msg, nonce, conn_info.send_key))
    |> Stlv.encode(@proto_msg[type])
    |> send_packet(conn_info)
  end

  def unpack_nonce_box(
        %{pkt: [{_, <<nonce::binary-size(24), box::binary>>} | _], recv_key: recv_key} = conn_info
      ) do
    case Kcl.secretunbox(box, nonce, recv_key) do
      :error ->
        Logger.error("Unboxing error")
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
        {:keep_state, %{conn_info | :wire => wire}, [{:next_event, :internal, :data}]}

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

        Logger.debug([
          dude,
          " unrequited wants: ",
          conn_info.want |> MapSet.size() |> Integer.to_string()
        ])
    end

    {:stop, :normal, %{}}
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

  defp arrow(:in), do: "→"
  defp arrow(:out), do: "←"

  defp log_traffic(conn_info, dir, type, data \\ []) do
    Enum.join(
      [
        tilde_peer(conn_info),
        arrow(dir),
        Atom.to_string(type),
        data |> length |> Integer.to_string()
      ],
      " "
    )
    |> Logger.debug()
  end

  defp tilde_peer(conn_info) do
    case Map.fetch(conn_info, :short_peer) do
      {:ok, them} -> them
      :error -> "~unknown"
    end
  end

  defp stored_info_map() do
    Baobab.stored_info() |> Enum.reduce(%{}, fn {a, l, e}, acc -> Map.put(acc, {a, l}, e) end)
  end
end
