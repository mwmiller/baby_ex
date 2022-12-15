defmodule Baby.Protocol do
  alias Baobab.ClumpMeta
  alias Baby.Util

  @moduledoc """
  Protocol implementation
  """

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

  @doc """
  A dual-way map between message types and semantic protocol atoms
  """
  def msglookup(val), do: Map.get(@proto_msg, val)

  @doc """
  A map of the protocol definitions
  """
  def definition(), do: @protodef

  @doc """
  Craft and enqueue an outbound messagei or the provided type from the current connection state
  """
  def outbound(conn_info, message_type)

  def outbound(conn_info, :HELLO) do
    %{secret: esk, public: epk} = :enacl.box_keypair()
    type = :HELLO

    (conn_info.our_pk <> epk <> :enacl.auth(conn_info.clump_id, epk))
    |> Stlv.encode(@proto_msg[type])
    |> enqueue_packet(conn_info, type)
    |> Map.merge(%{our_epk: epk, our_esk: esk})
  end

  def outbound(conn_info, :AUTH) do
    send_key =
      :enacl.curve25519_scalarmult(
        conn_info.our_esk,
        :enacl.crypto_sign_ed25519_public_to_curve25519(conn_info.their_pk)
      )
      |> Blake2.hash2b(32)

    recv_key =
      :enacl.curve25519_scalarmult(
        :enacl.crypto_sign_ed25519_secret_to_curve25519(conn_info.our_sk <> conn_info.our_pk),
        conn_info.their_epk
      )
      |> Blake2.hash2b(32)

    nci = Map.merge(conn_info, %{:recv_key => recv_key, :send_key => send_key})

    (conn_info.clump_id <> recv_key)
    |> :enacl.sign_detached(conn_info.our_sk <> conn_info.our_pk)
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

  @doc """
  Handle inbound data of the provided message type based on the supplied connection state
  """
  def inbound(data, conn_info, message_type)

  def inbound(data, conn_info, :HAVE) do
    with {cbor, new_conn} <- unpack_nonce_box(data, conn_info),
         {:ok, decoded, ""} <- CBOR.decode(cbor) do
      decoded
      |> ClumpMeta.filter_blocked(new_conn.clump_id)
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
         true <- :enacl.auth_verify(hmac, conn_info.clump_id, their_epk) do
      peer = their_pk |> Baobab.Identity.as_base62()
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
         true <- :enacl.sign_verify_detached(sig, nci.clump_id <> nci.send_key, nci.their_pk),
         false <- ClumpMeta.blocked?(nci.their_pk, conn_info.clump_id) do
      Util.connection_log(conn_info, :both, "connected", :info)

      us = Enum.map([nci.our_pk, nci.their_pk], fn k -> Baobab.Identity.as_base62(k) end)

      nci
      |> Map.drop([
        :our_pk,
        :our_sk,
        :our_esk,
        :our_epk,
        :their_pk,
        :their_epk
      ])
      |> Map.merge(%{us_fun: fn a -> a in us end})
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
        # If we've lost our own logs, try to get everything
        we_have == 0 ->
          case conn_info.us_fun.(a) do
            false -> [{a, l, e}]
            true -> [{a, l}]
          end

        we_have < e ->
          [{a, l, we_have + 1, e}]

        # caught up, maybe fill in some missing bits this pass
        true ->
          missing_bits([a, l, e], conn_info.clump_id)
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
    # Break up large requests which are not full logs
    case e - s >= 11 do
      true ->
        m = div(s + e, 2)
        gather_our([[a, l, s, m], [a, l, m + 1, e]] ++ rest, conn_info, todo)

      false ->
        gather_our(rest, conn_info, [[a, l, s, e] | todo])
    end
  end

  defp pull_log_data([a, l], conn_info),
    do: Baobab.full_log(a, log_id: l, clump_id: conn_info.clump_id, format: :binary)

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
    stuff
    |> Baobab.Interchange.import_binaries(clump_id: conn_info.clump_id)
    |> import_summary(conn_info)
  end

  defp import_summary([], conn_info), do: conn_info

  defp import_summary([{:error, reason} | rest], conn_info) do
    Util.connection_log(conn_info, :in, "import error:" <> reason, :warn)
    import_summary(rest, conn_info)
  end

  # They have to be provided in order or the chain won't verify
  # There are extra updates here, but maybe there's an error mixed in
  defp import_summary([%Baobab.Entry{author: author, log_id: l, seqnum: e} | rest], conn_info) do
    a = author |> Baobab.Identity.as_base62()

    import_summary(rest, %{
      conn_info
      | have: Map.merge(conn_info.have, %{{a, l} => e}),
        want: Map.drop(conn_info.want, [{a}, {a, l}, {a, l, e}])
    })
  end

  # Do not bother sending empty arrays
  defp encode_replication([], conn_info, _), do: conn_info
  # Skip Baobab error conditions
  defp encode_replication({:error, _}, conn_info, _), do: conn_info
  defp encode_replication(:error, conn_info, _), do: conn_info

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
        case :enacl.secretbox_open(box, nonce, conn_info.recv_key) do
          {:ok, msg} ->
            {msg, %{conn_info | their_nonces: MapSet.put(conn_info.their_nonces, nonce)}}

          _ ->
            Util.connection_log(conn_info, :in, "unboxing error", :error)
            :unbox
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

    (nonce <> :enacl.secretbox(msg, nonce, conn_info.send_key))
    |> Stlv.encode(@proto_msg[type])
    |> enqueue_packet(conn_info, st)
  end

  defp enqueue_packet(packet, ci, type), do: %{ci | outbox: ci.outbox ++ [{packet, type}]}
end
