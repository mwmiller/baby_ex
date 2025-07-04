defmodule Baby.Protocol do
  alias Baobab.ClumpMeta
  alias Baby.Util
  alias Baby.Log.Acceptor

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

  def outbound(%{clump_id: clump_id} = conn_info, :HAVE) do
    data =
      case Map.get(conn_info, :added) do
        nil ->
          clump_id
          |> Baobab.stored_info()

        added ->
          added
      end

    encode_replication(data, Map.drop(conn_info, [:added]), :HAVE)
  end

  def outbound(%{want: want} = conn_info, :WANT), do: encode_replication(want, conn_info, :WANT)

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
      |> want_their(conn_info, [])
      |> outbound(:WANT)
      |> Map.drop([:want])
    else
      e -> Util.log_fatal(conn_info, e)
    end
  end

  def inbound(data, conn_info, :WANT) do
    with {cbor, new_conn} <- unpack_nonce_box(data, conn_info),
         {:ok, decoded, ""} <- CBOR.decode(cbor) do
      decoded
      |> gather_our(new_conn, [])
    else
      e -> Util.log_fatal(conn_info, e)
    end
  end

  def inbound(data, conn_info, :BAMB) do
    with {cbor, new_conn} <- unpack_nonce_box(data, conn_info),
         {:ok, decoded, ""} <- CBOR.decode(cbor) do
      Acceptor.add_job(decoded, new_conn)
      new_conn
    else
      e -> Util.log_fatal(conn_info, e)
    end
  end

  def inbound(data, conn_info, :HELLO) do
    with {1, hello} <- data,
         <<their_pk::binary-size(32), their_epk::binary-size(32), hmac::binary-size(32)>> <-
           hello,
         true <- Kcl.valid_auth?(hmac, their_epk, conn_info.clump_id) do
      peer = their_pk |> Baobab.Identity.as_base62()
      short_peer = "~" <> (peer |> String.slice(0..6))

      Map.merge(conn_info, %{
        short_peer: short_peer,
        peer: peer,
        their_pk: their_pk,
        their_epk: their_epk
      })
    else
      e -> Util.log_fatal(conn_info, e)
    end
  end

  def inbound(data, conn_info, :AUTH) do
    with {sig, nci} <- unpack_nonce_box(data, conn_info),
         true <- Kcl.valid_signature?(sig, nci.clump_id <> nci.send_key, nci.their_pk),
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
      |> Map.merge(%{us_fun: fn index -> elem(index, 0) in us end})
    else
      e -> Util.log_fatal(conn_info, e)
    end
  end

  defp want_their([], conn_info, acc),
    do: Map.merge(conn_info, %{want: sort_wants(acc, conn_info)})

  defp want_their([[a, l, e] | rest], %{clump_id: clump_id} = conn_info, acc) do
    we_have = Baobab.max_seqnum(a, log_id: l, clump_id: clump_id)

    add =
      cond do
        we_have == 0 ->
          [{a, l, e}]

        we_have < e ->
          [{a, l, we_have + 1, e}]

        # caught up, maybe fill in some missing bits this pass
        true ->
          missing_bits([a, l, e], clump_id)
      end

    want_their(rest, conn_info, acc ++ add)
  end

  defp sort_wants(want, %{us_fun: uf, send_key: <<sok, _::binary>>}) do
    # sok is effectively random per connection
    want
    |> presort(<<sok>>)
    |> Enum.split_with(fn i -> uf.(i) end)
    |> then(fn {hi, lo} -> hi ++ lo end)
  end

  # element 0 is the author, element 1 is the log_id
  # we decides against which element we sort first
  # fo decides the order for the first element sorted
  # so decides the order for the second element sorted
  # dr decides whether to rotate the list
  def presort(wants, <<we::1, fo::1, so::1, _dr::1, _::bitstring>>) do
    {first, second} = which_elem(we)

    wants
    |> Enum.sort_by(fn t -> elem(t, first) end, which_order(fo))
    |> Enum.sort_by(fn t -> elem(t, second) end, which_order(so))
    |> Enum.sort_by(&tuple_size/1, :asc)
  end

  # author then log_id
  defp which_elem(0), do: {0, 1}
  # log_id then author
  defp which_elem(1), do: {1, 0}

  defp which_order(0), do: :asc
  defp which_order(1), do: :desc

  # Do not "rotate"
  # defp maybe_rotate(list, 0), do: list

  # "rotate" the middle to the front
  # defp maybe_rotate(list, 1) do
  #  {front, back} = Enum.split(list, list |> length |> div(2))
  #  back ++ front
  # end

  defp missing_bits([a, l, e], clump_id) do
    MapSet.new(1..e)
    |> MapSet.difference(MapSet.new(Baobab.all_seqnum(a, log_id: l, clump_id: clump_id)))
    |> Util.range_points()
    |> Enum.map(fn {s, e} -> {a, l, s, e} end)
  end

  defp gather_our([], %{shoots: shoots} = conn_info, todo) do
    ns = todo |> Enum.reverse() |> then(fn t -> shoots ++ t end) |> Enum.uniq()

    Map.merge(conn_info, %{shoots: ns})
  end

  # Full logs for author
  defp gather_our([[a] | rest], %{clump_id: clump_id} = conn_info, todo) do
    clump_id
    |> Baobab.stored_info()
    |> Enum.reduce([], fn entry, acc ->
      case entry do
        {^a, l, _} -> [[a, l] | acc]
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
    case e - s >= 19 do
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
        Util.connection_log(conn_info, :in, "possible replay attack via reused nonce", :warning)
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
