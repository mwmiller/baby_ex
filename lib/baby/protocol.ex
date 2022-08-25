defmodule Baby.Protocol do
  require Logger
  alias Baby.Connection
  @protodef %{0 => :BYE, 1 => :HELLO, 2 => :AUTH, 5 => :HAVE, 6 => :WANT, 8 => :BAMB}
  @proto_msg Map.merge(
               @protodef,
               @protodef |> Map.to_list() |> Map.new(fn {k, v} -> {v, k} end)
             )

  def msglookup(val), do: Map.fetch!(@proto_msg, val)

  def outbound(conn_info, :HELLO) do
    {esk, epk} = Kcl.generate_key_pair(:encrypt)
    type = :HELLO

    (conn_info.our_pk <> epk <> Kcl.auth(epk, conn_info.clump_id))
    |> Stlv.encode(@proto_msg[type])
    |> enqueue_packet(conn_info, type)
    |> Map.merge(%{our_epk: epk, our_esk: esk})
  end

  def outbound(conn_info, :AUTH) do
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
    |> pack_and_ship_nonce_box(nci, :AUTH)
  end

  def outbound(conn_info, :HAVE) do
    conn_info.have
    |> Map.to_list()
    |> Enum.map(fn {{a, l}, e} -> {a, l, e} end)
    |> encode_replication(conn_info, :HAVE)
  end

  def outbound(conn_info, :WANT) do
    conn_info.want
    |> Map.keys()
    |> encode_replication(conn_info, :WANT)
  end

  def outbound(conn_info, :BAMB) do
    Enum.reduce(conn_info.shoots, conn_info, fn e, _a ->
      encode_replication(e, conn_info, :BAMB)
    end)
  end

  def inbound(data, conn_info, :HAVE) do
    with {cbor, new_conn} <- unpack_nonce_box(data, conn_info),
         {:ok, decoded, ""} <- CBOR.decode(cbor) do
      decoded
      |> want_their(new_conn, [])
      |> outbound(:WANT)
    else
      _ -> :error
    end
  end

  def inbound(data, conn_info, :WANT) do
    with {cbor, new_conn} <- unpack_nonce_box(data, conn_info),
         {:ok, decoded, ""} <- CBOR.decode(cbor) do
      decoded
      |> gather_our(new_conn)
      |> outbound(:BAMB)
    else
      _ -> :error
    end
  end

  def inbound(data, conn_info, :BAMB) do
    with {cbor, new_conn} <- unpack_nonce_box(data, conn_info),
         {:ok, decoded, ""} <- CBOR.decode(cbor) do
      import_their(decoded, new_conn)
    else
      _ -> :error
    end
  end

  def inbound(data, conn_info, :HELLO) do
    with {1, hello} <- data,
         <<their_pk::binary-size(32), their_epk::binary-size(32), hmac::binary-size(32)>> <-
           hello,
         true <- Kcl.valid_auth?(hmac, their_epk, conn_info.clump_id) do
      peer = their_pk |> Baobab.b62identity()
      short_peer = "~" <> (peer |> String.slice(0..6))

      Map.merge(conn_info, %{
        short_peer: short_peer,
        peer: peer,
        their_pk: their_pk,
        their_epk: their_epk
      })
    else
      _ -> :error
    end
  end

  def inbound(data, conn_info, :AUTH) do
    with {sig, nci} <- unpack_nonce_box(data, conn_info),
         true <- Kcl.valid_signature?(sig, nci.clump_id <> nci.send_key, nci.their_pk) do
      Map.drop(nci, [
        :our_pk,
        :our_sk,
        :our_esk,
        :our_epk,
        :their_pk,
        :their_epk
      ])
    else
      _ -> :error
    end
  end

  defp want_their([], conn_info, wants) do
    # When talking directly to the source, get as much
    # as one can of their logs.
    Map.merge(conn_info, %{
      want: Map.merge(conn_info.want, reduce_wants(wants ++ [{conn_info.peer}]))
    })
  end

  defp want_their([[a, l, e] | rest], conn_info, acc) do
    we_have = Map.get(conn_info.have, {a, l}, 0)

    add =
      cond do
        # It's new to us, get everything we can
        we_have == 0 -> [{a, l}]
        # catch up
        we_have < e -> [{a, l, we_have + 1, e}]
        # We're even or ahead -- we assume they'll ask if they want more
        we_have >= e -> []
      end

    want_their(rest, conn_info, acc ++ add)
  end

  # Bad time complexity all up in here.
  defp reduce_wants(wants), do: wants |> Enum.sort() |> Enum.uniq() |> reduce_wants([])
  defp reduce_wants([], acc), do: acc |> Enum.reduce(%{}, fn e, a -> Map.put(a, e, true) end)

  # Full logs for author means no need for partials
  # or individual logs
  defp reduce_wants([{a} | rest], acc) do
    reduce_wants(
      Enum.reject(rest, fn
        {^a, _} -> true
        {^a, _, _} -> true
        {^a, _, _, _} -> true
        _ -> false
      end),
      [{a} | acc]
    )
  end

  # Full log means no need for partials
  defp reduce_wants([{a, l} | rest], acc) do
    reduce_wants(
      Enum.reject(rest, fn
        {^a, ^l, _} -> true
        {^a, ^l, _, _} -> true
        _ -> false
      end),
      [{a, l} | acc]
    )
  end

  # We're allow to want two different partial chains
  # Since we've sorted this we can move everything at once
  defp reduce_wants(partials, acc) do
    reduce_wants([], partials ++ acc)
  end

  defp gather_our([], conn_info), do: %{conn_info | shoots: Enum.reverse(conn_info.shoots)}

  # Full logs for author
  defp gather_our([[a] | rest], conn_info) do
    conn_info.have
    |> Map.keys()
    |> Enum.reduce([], fn entry, acc ->
      case entry do
        {^a, l} -> [[a, l] | acc]
        _ -> acc
      end
    end)
    |> then(fn al -> rest ++ al end)
    |> gather_our(conn_info)
  end

  # Full log for author log_id
  defp gather_our([[a, l] | rest], conn_info) do
    nci =
      case Baobab.full_log(a, log_id: l, format: :binary) do
        [] -> conn_info
        entries -> %{conn_info | shoots: [entries | conn_info.shoots]}
      end

    gather_our(rest, nci)
  end

  # Full chain from 1 to requested entry
  defp gather_our([[a, l, e] | rest], conn_info) do
    nci =
      case Baobab.log_at(a, e, log_id: l, format: :binary) do
        [] -> conn_info
        entries -> %{conn_info | shoots: [entries | conn_info.shoots]}
      end

    gather_our(rest, nci)
  end

  # Chain links from start to end
  defp gather_our([[a, l, s, e] | rest], conn_info) do
    nci =
      case Baobab.log_range(a, {s, e}, log_id: l, format: :binary) do
        [] -> conn_info
        entries -> %{conn_info | shoots: [entries | conn_info.shoots]}
      end

    gather_our(rest, nci)
  end

  defp import_their(stuff, conn_info) do
    stuff |> Baobab.import() |> import_summary(conn_info)
  end

  defp import_summary([], conn_info), do: conn_info

  defp import_summary([{:error, reason} | rest], conn_info) do
    Enum.join(
      [Connection.tilde_peer(conn_info), Connection.arrow(:in), "import error:", reason],
      " "
    )
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
        want: Map.drop(conn_info.want, [{a}, {a, l}, {a, l, e}])
    })
  end

  # Do not bother sending empty arrays
  defp encode_replication([], conn_info, _), do: conn_info

  defp encode_replication(msg, conn_info, type) do
    msg
    |> CBOR.encode()
    |> pack_and_ship_nonce_box(conn_info, type)
  end

  def unpack_nonce_box({_, <<nonce::binary-size(24), box::binary>>}, conn_info) do
    case MapSet.member?(conn_info.their_nonces, nonce) do
      true ->
        Logger.warn([Connection.tilde_peer(conn_info), " possible replay attack via reused nonce"])

        :replay

      false ->
        case Kcl.secretunbox(box, nonce, conn_info.recv_key) do
          :error ->
            Logger.error([Connection.tilde_peer(conn_info), " unboxing error"])
            :unbox

          msg ->
            {msg, %{conn_info | their_nonces: MapSet.put(conn_info.their_nonces, nonce)}}
        end
    end
  end

  defp pack_and_ship_nonce_box(msg, conn_info, type, wrapped_type \\ nil) do
    nonce = :rand.bytes(24)

    st =
      case wrapped_type do
        nil -> type
        wt -> wt
      end

    (nonce <> Kcl.secretbox(msg, nonce, conn_info.send_key))
    |> Stlv.encode(@proto_msg[type])
    |> enqueue_packet(conn_info, st)
  end

  defp enqueue_packet(packet, ci, type), do: %{ci | outbox: ci.outbox ++ [{packet, type}]}
end
