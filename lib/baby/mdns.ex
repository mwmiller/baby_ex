defmodule Baby.Mdns do
  import MdnsLite.DNS

  alias MdnsLite.DNS

  require Logger

  @moduledoc """
  Local network peer visibility and rendezvous via mDNS/DNS-SD

  A clump may make itself visible to other `Baby` peers on the local
  network by announcing its listener as a `_bushbaby._tcp` service with
  its `clump_id` in the TXT record.  Conversely, a "meta cryout" (see
  `Baby.Monitor`) periodically browses for such services and opens
  replication connections to the peers it sees, exactly as it would to
  statically configured cryout targets.

  Announcing is delegated to the pure-Erlang-stack `mdns_lite`
  responder; browsing is done here via our own multicast socket so that
  multiple instances on one machine can find each other.
  """

  @mdns_group {224, 0, 0, 251}
  @mdns_port 5353
  @service_protocol "bushbaby"
  @default_browse_ms 1000

  @typedoc "A discovered bushbaby service instance"
  @type peer :: %{
          optional(:instance) => String.t(),
          ip: :inet.ip_address(),
          port: :inet.port_number(),
          txt: %{String.t() => String.t()}
        }

  @doc """
  Advertise a clump's listener as a `_bushbaby._tcp` service

  Requires that the `:mdns_lite` application has been started.  The
  advertised instance name defaults to "<hostname>-<clump_id>" and may
  be overridden with the `:instance` option.

      Baby.Mdns.announce("Quagga", 8483)
      Baby.Mdns.announce("Quagga", 8483, instance: "matt's quagga")
  """
  @spec announce(String.t(), :inet.port_number(), Keyword.t()) :: :ok
  def announce(clump_id, port, opts \\ []) when is_binary(clump_id) and is_integer(port) do
    instance = Keyword.get(opts, :instance, default_instance(clump_id))

    service = %{
      id: String.to_atom("baby_" <> clump_id),
      instance_name: instance,
      protocol: @service_protocol,
      transport: "tcp",
      port: port,
      txt_payload: %{clump_id: clump_id}
    }

    :ok = MdnsLite.add_mdns_service(service)
    Logger.info("Announcing #{instance}._#{@service_protocol}._tcp.local on port #{port}")
  end

  @doc """
  Browse the local network for announced bushbaby services

  Sends a PTR query for `<service>._tcp.local`, repeating it part-way
  through the window to tolerate lost datagrams and responders that
  are still starting up, and collects responses until `:timeout`
  milliseconds have passed (default: 1000).  Returns all discovered
  instances, including possibly our own.
  """
  @spec browse(Keyword.t()) :: [peer()]
  def browse(opts \\ []) do
    timeout = Keyword.get(opts, :timeout, @default_browse_ms)
    domain = to_charlist("_" <> @service_protocol <> "._tcp.local")

    # Loopback delivery is enabled so that responders on this same host
    # hear the query; they reply unicast to our ephemeral port
    # (RFC 6762 section 6), so no multicast group membership is needed.
    {:ok, sock} =
      :gen_udp.open(0, [:binary, active: true, multicast_ttl: 255, multicast_loop: true])

    packet =
      DNS.encode(
        dns_rec(
          header: dns_header(id: 0, qr: false),
          qdlist: [dns_query(class: :in, type: :ptr, domain: domain)]
        )
      )

    start = System.monotonic_time(:millisecond)
    deadline = start + timeout

    # Repeat the query halfway through the window (capped so short
    # browses do not degenerate into two immediate sends)
    resend_at = start + min(div(timeout, 2), 750)

    :ok = :gen_udp.send(sock, @mdns_group, @mdns_port, packet)
    records = collect(sock, deadline, resend_at, packet)
    :gen_udp.close(sock)

    peers_from(records)
  end

  @doc """
  Browse for peers of a specific clump, excluding ourselves

  Returns only those discovered instances whose TXT record declares
  `clump_id=<clump_id>` and whose `{ip, port}` does not match this
  node's own listener (`:port` option).  Multiple instances at the same
  address are collapsed into one entry.
  """
  @spec peers(String.t(), Keyword.t()) :: [%{ip: :inet.ip_address(), port: :inet.port_number()}]
  def peers(clump_id, opts \\ []) when is_binary(clump_id) do
    our_port = Keyword.get(opts, :port)

    browse(opts)
    |> Enum.filter(fn peer -> peer.txt["clump_id"] == clump_id end)
    |> drop_self(our_port)
    |> Enum.uniq_by(&{&1.ip, &1.port})
  end

  # Assemble service instances from the PTR records seen and their
  # companion SRV/TXT/A records
  @doc false
  def peers_from(records) do
    records
    |> instances()
    |> Enum.map(&peer_at(records, &1))
    |> Enum.reject(&is_nil/1)
  end

  defp instances(records) do
    for dns_rr(type: :ptr, data: inst) <- records, uniq: true do
      inst
    end
  end

  defp peer_at(_records, nil), do: nil

  defp peer_at(records, instance) do
    case srv_for(records, instance) do
      nil ->
        nil

      {port, target} ->
        txt = txt_for(records, instance)

        case addr_for(records, target) do
          nil -> nil
          ip -> %{instance: short_name(instance), ip: ip, port: port, txt: txt}
        end
    end
  end

  defp srv_for(records, instance) do
    case Enum.find(records, &match?(dns_rr(type: :srv, domain: ^instance), &1)) do
      dns_rr(data: {_priority, _weight, port, target}) -> {port, strip_dot(target)}
      _ -> nil
    end
  end

  defp strip_dot(domain) do
    case List.last(domain || []) do
      ?. -> List.delete_at(domain, -1)
      _ -> domain || []
    end
  end

  # Instance names arrive as full service DNS names; we surface just the
  # human-readable instance label
  defp short_name(instance) do
    name = to_string(instance)
    suffix = "._" <> @service_protocol <> "._tcp.local"
    String.replace_suffix(name, suffix, "")
  end

  defp txt_for(records, instance) do
    case Enum.find(records, &match?(dns_rr(type: :txt, domain: ^instance), &1)) do
      dns_rr(data: data) -> txt_map(data)
      _ -> %{}
    end
  end

  defp addr_for(records, target) do
    case Enum.find(records, &match?(dns_rr(type: :a, domain: ^target), &1)) do
      dns_rr(data: data) when is_tuple(data) -> data
      _ -> nil
    end
  end

  # TXT items arrive as binaries over the wire but as charlists when
  # read from mdns_lite's local table; normalize both
  @doc false
  def txt_map(items) do
    Map.new(items, fn item ->
      case item |> IO.iodata_to_binary() |> String.split("=", parts: 2) do
        [k] -> {k, ""}
        [k, v] -> {k, v}
      end
    end)
  end

  defp collect(sock, deadline, resend_at, packet) do
    now = System.monotonic_time(:millisecond)

    if now >= deadline do
      []
    else
      next_stop = min(deadline, resend_at)

      receive do
        {:udp, ^sock, _ip, _port, data} ->
          case DNS.decode(data) do
            {:ok, dns_rec(anlist: anlist, arlist: arlist)} ->
              anlist ++ arlist ++ collect(sock, deadline, resend_at, packet)

            _ ->
              collect(sock, deadline, resend_at, packet)
          end
      after
        next_stop - now ->
          if resend_at <= now or resend_at > deadline do
            []
          else
            :ok = :gen_udp.send(sock, @mdns_group, @mdns_port, packet)
            collect(sock, deadline, :infinity, packet)
          end
      end
    end
  end

  # Drop any peer sitting on one of this host's addresses at our
  # listener's port -- i.e., ourselves
  @doc false
  def drop_self(peers, nil), do: peers
  def drop_self(peers, our_port), do: drop_self(peers, our_port, our_ipv4s())

  def drop_self(peers, our_port, ours) when is_integer(our_port) do
    ours = MapSet.new(ours, &{&1, our_port})
    Enum.reject(peers, fn peer -> {peer.ip, peer.port} in ours end)
  end

  @doc false
  def our_ipv4s do
    {:ok, ifaddrs} = :inet.getifaddrs()

    for {_name, opts} <- ifaddrs,
        up?(opts),
        {:addr, {_, _, _, _} = ip} <- opts do
      ip
    end
    |> Enum.uniq()
  end

  defp up?(opts) do
    case List.keyfind(opts, :flags, 0) do
      {:flags, flags} -> :up in flags
      _ -> false
    end
  end

  defp default_instance(clump_id) do
    {:ok, hostname} = :inet.gethostname()
    "#{hostname}-#{clump_id}"
  end
end
