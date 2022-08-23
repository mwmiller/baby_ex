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
    |> Connection.send_packet(conn_info, type)

    Map.merge(conn_info, %{our_epk: epk, our_esk: esk})
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
    |> Connection.pack_and_ship_nonce_box(nci, :AUTH)

    nci
  end

  def outbound(conn_info, :HAVE) do
    conn_info.have
    |> Map.to_list()
    |> Enum.map(fn {{a, l}, e} -> {a, l, e} end)
    |> encode_replication(:HAVE, conn_info)
  end

  def inbound(data, conn_info, :HAVE), do: request_their(data, conn_info, [])
  def inbound(data, conn_info, :WANT), do: send_our(data, conn_info)
  def inbound(data, conn_info, :BAMB), do: import_their(data, conn_info)

  defp request_their([], conn_info, wants) do
    # When talking directly to the source, get as much
    # as one can of their logs.
    short_map = Map.merge(conn_info.want, reduce_wants(wants ++ [{conn_info.peer}]))
    encode_replication(Map.keys(short_map), :WANT, %{conn_info | want: short_map})
  end

  defp request_their([[a, l, e] | rest], conn_info, acc) do
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

    request_their(rest, conn_info, acc ++ add)
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

  # Full chain from 1 to requested entry
  defp send_our([[a, l, e] | rest], conn_info) do
    nci =
      case Baobab.log_at(a, e, log_id: l, format: :binary) do
        [] -> conn_info
        entries -> encode_replication(entries, :BAMB, conn_info)
      end

    send_our(rest, nci)
  end

  # Chain links from start to end
  defp send_our([[a, l, s, e] | rest], conn_info) do
    nci =
      case Baobab.log_range(a, {s, e}, log_id: l, format: :binary) do
        [] -> conn_info
        entries -> encode_replication(entries, :BAMB, conn_info)
      end

    send_our(rest, nci)
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
  defp encode_replication([], _, conn_info), do: conn_info

  defp encode_replication(msg, type, conn_info) do
    msg
    |> CBOR.encode()
    |> Connection.pack_and_ship_nonce_box(conn_info, type)

    conn_info
  end
end
