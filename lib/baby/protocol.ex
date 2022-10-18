defmodule Baby.Protocol do
  alias Baby.Util

  @protodef %{
    :HELLO => %{type: 1, instate: :hello, outstate: :auth},
    :AUTH => %{type: 2, instate: :auth, outstate: :replicate},
    :HAVE => %{type: 5, instate: :replicate, outstate: :replicate},
    :WANT => %{type: 6, instate: :replicate, outstate: :replicate},
    :BAMB => %{type: 8, instate: :replicate, outstate: :replicate}
  }
  @proto_msg @protodef
             |> Map.to_list()
             |> Enum.reduce(%{}, fn {k, %{type: n}}, a -> Map.merge(a, %{k => n, n => k}) end)

  def msglookup(val), do: Map.get(@proto_msg, val)
  def definition(), do: @protodef

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

  def outbound(%{shoots: []} = conn_info, :BAMB), do: conn_info

  def outbound(%{shoots: [s | rest]} = conn_info, :BAMB) do
    s
    |> pull_log_data(conn_info)
    |> encode_replication(conn_info, :BAMB)
    |> Map.merge(%{shoots: rest})
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
      |> gather_our(new_conn, [])
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
      Util.connection_log(conn_info, :both, "connected", :info)

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
    # At some point I thought that the symmtery with WANT Map
    # made sense.  I surely knew about MapSet.  I must rediscover the why
    # here.  It will be easier to do rules-based set operations with MapSets
    mapped_wants = Enum.reduce(wants, %{}, fn e, a -> Map.put(a, e, true) end)
    Map.merge(conn_info, %{want: Map.merge(conn_info.want, mapped_wants)})
  end

  defp want_their([[a, l, e] | rest], conn_info, acc) do
    we_have = Map.get(conn_info.have, {a, l}, 0)

    add =
      cond do
        we_have == 0 -> [{a, l, e}]
        we_have < e -> [{a, l, we_have + 1, e}]
        # caught up, maybe fill in some missing bits this pass
        true -> missing_bits([a, l, e], conn_info.clump_id)
      end

    want_their(rest, conn_info, acc ++ add)
  end

  defp missing_bits([a, l, e], clump_id) do
    MapSet.new(1..e)
    |> MapSet.difference(MapSet.new(Baobab.all_seqnum(a, log_id: l, clump_id: clump_id)))
    |> Util.range_points()
    |> Enum.map(fn {s, e} -> {a, l, s, e} end)
  end

  defp gather_our([], conn_info, todo),
    do: %{conn_info | shoots: todo |> Enum.sort() |> Enum.uniq()}

  # Full logs for author
  defp gather_our([[a] | rest], conn_info, todo) do
    conn_info.have
    |> Map.keys()
    |> Enum.reduce([], fn entry, acc ->
      case entry do
        {^a, l} -> [[a, l] | acc]
        _ -> acc
      end
    end)
    |> then(fn al -> rest ++ al end)
    |> gather_our(conn_info, todo)
  end

  # Full log for author log_id
  # This can get big so we will figure out how big and
  # send it on
  defp gather_our([[a, l] | rest], conn_info, todo) do
    max = Baobab.max_seqnum(a, log_id: l, clump_id: conn_info.clump_id)
    gather_our([[a, l, 1, max] | rest], conn_info, todo)
  end

  # Full chain from 1 to requested entry passes unscathed
  # Chain is logarithmic in sequence number
  defp gather_our([[a, l, e] | rest], conn_info, todo),
    do: gather_our(rest, conn_info, [[a, l, e] | todo])

  defp gather_our([[a, l, s, e] | rest], conn_info, todo) do
    # Break up large requests
    case e - s >= 7 do
      true ->
        m = div(s + e, 2)
        gather_our([[a, l, s, m], [a, l, m + 1, e]] ++ rest, conn_info, todo)

      false ->
        gather_our(rest, conn_info, [[a, l, s, e] | todo])
    end
  end

  defp pull_log_data([a, l, e], conn_info),
    do: Baobab.log_at(a, e, log_id: l, clump_id: conn_info.clump_id, format: :binary)

  defp pull_log_data([a, l, s, e], conn_info),
    do:
      Baobab.log_range(a, {s, e},
        log_id: l,
        clump_id: conn_info.clump_id,
        format: :binary
      )

  # We've munged everything into one of the above types
  defp pull_log_data(_, _), do: []

  defp import_their(stuff, conn_info) do
    stuff |> Baobab.import(clump_id: conn_info.clump_id) |> import_summary(conn_info)
  end

  defp import_summary([], conn_info), do: conn_info

  defp import_summary([{:error, reason} | rest], conn_info) do
    Util.connection_log(conn_info, :in, "import error:" <> reason, :warn)
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
        Util.connection_log(conn_info, :in, "possible replay attack via reused nonce", :warn)
        :replay

      false ->
        case Kcl.secretunbox(box, nonce, conn_info.recv_key) do
          :error ->
            Util.connection_log(conn_info, :in, "unboxing error", :error)
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
