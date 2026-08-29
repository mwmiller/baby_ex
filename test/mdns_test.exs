defmodule Baby.MdnsTest do
  use ExUnit.Case, async: false
  import ExUnit.CaptureLog
  import MdnsLite.DNS

  alias Baby.Mdns

  setup_all do
    # mdns_lite logs it chose en0/utun, etc. on startup; keep that out of the
    # test runner's output
    {{:ok, _}, _} = with_log(fn -> Application.ensure_all_started(:mdns_lite) end)
    :ok
  end

  @service_domain ~c"_bushbaby._tcp.local"
  @instance ~c"testhost._bushbaby._tcp.local"

  defp records do
    [
      dns_rr(domain: @service_domain, class: :in, type: :ptr, ttl: 120, data: @instance),
      dns_rr(
        domain: @instance,
        class: :in,
        type: :srv,
        ttl: 120,
        data: {0, 0, 8483, ~c"testhost.local."}
      ),
      dns_rr(
        domain: @instance,
        class: :in,
        type: :txt,
        ttl: 120,
        data: [~c"clump_id=Quagga", ~c"note=hello"]
      ),
      dns_rr(domain: ~c"testhost.local", class: :in, type: :a, ttl: 120, data: {192, 168, 1, 7})
    ]
  end

  describe "txt_map/1" do
    test "parses binary items as delivered over the wire" do
      assert %{"clump_id" => "Quagga", "note" => ""} = Mdns.txt_map(["clump_id=Quagga", "note"])
    end

    test "parses charlist items as stored in mdns_lite's local table" do
      assert %{"clump_id" => "Quagga"} = Mdns.txt_map([~c"clump_id=Quagga"])
    end

    test "keeps only the first equals sign in a value" do
      assert %{"k" => "a=b"} = Mdns.txt_map(["k=a=b"])
    end

    test "empty input yields an empty map" do
      assert %{} == Mdns.txt_map([])
    end
  end

  describe "peers_from/1" do
    test "assembles a peer from PTR/SRV/TXT/A records" do
      assert [%{instance: "testhost"}] =
               peers = Mdns.peers_from(records())

      assert [%{ip: {192, 168, 1, 7}, port: 8483, txt: %{"clump_id" => "Quagga"}}] = peers
    end

    test "an instance with no SRV record is ignored" do
      [ptr | rest] = records()

      assert [] == Mdns.peers_from([ptr | Enum.drop(rest, 0) |> Enum.reject(&match_srv/1)])
    end

    test "an instance with no A record is ignored" do
      no_a = records() |> Enum.reject(&match_a/1)

      assert [] == Mdns.peers_from(no_a)
    end

    test "unrelated records are ignored" do
      assert [] ==
               Mdns.peers_from([
                 dns_rr(domain: ~c"other.local", class: :in, type: :a, data: {1, 2, 3, 4}),
                 dns_rr(domain: @instance, class: :in, type: :cname, data: ~c"x")
               ])
    end

    test "handles multiple instances at once" do
      inst_b = ~c"hostb._bushbaby._tcp.local"

      multi =
        records() ++
          [
            dns_rr(domain: @service_domain, class: :in, type: :ptr, data: inst_b),
            dns_rr(domain: inst_b, class: :in, type: :srv, data: {0, 0, 9999, ~c"hostb.local."}),
            dns_rr(domain: ~c"hostb.local", class: :in, type: :a, data: {10, 1, 2, 3})
          ]

      found = Mdns.peers_from(multi)

      assert length(found) == 2
      assert Enum.any?(found, &(&1.ip == {10, 1, 2, 3} and &1.port == 9999))
    end
  end

  describe "drop_self/3" do
    @peer_self %{instance: "me", ip: {192, 168, 1, 5}, port: 8483, txt: %{}}
    @peer_them %{instance: "them", ip: {192, 168, 1, 9}, port: 8483, txt: %{}}
    @peer_other_port %{instance: "sibling", ip: {192, 168, 1, 5}, port: 8485, txt: %{}}

    test "drops our own address/port pair only" do
      ours = [{127, 0, 0, 1}, {192, 168, 1, 5}]
      peers = [@peer_self, @peer_them, @peer_other_port]

      kept = Mdns.drop_self(peers, 8483, ours)

      assert length(kept) == 2
      assert Enum.any?(kept, &(&1.instance == "them"))
      assert Enum.any?(kept, &(&1.instance == "sibling"))
    end

    test "keeps everything when no port is supplied" do
      assert [@peer_self] == Mdns.drop_self([@peer_self], nil)
    end
  end

  describe "our_ipv4s/0" do
    test "returns IPv4 tuples for up interfaces" do
      ips = Mdns.our_ipv4s()

      assert is_list(ips)
      assert {_, _, _, _} = hd(ips)
      # Loopback is always up and always ours
      assert {127, 0, 0, 1} in ips
    end
  end

  describe "announce/browse integration" do
    test "announced services are discovered by browsing" do
      port_a = ephemeral_port()
      port_b = ephemeral_port()

      # Announcing and browsing are intentionally noisy; the assertions all
      # operate on the results, so fold the Logger.info chatter away
      capture_log(fn ->
        :ok = Mdns.announce("MdnsTestClump", port_a, instance: "mdns-test-a")
        :ok = Mdns.announce("OtherClump", port_b, instance: "mdns-test-b")

        browse_opts = [timeout: 1500]
        found = Mdns.browse(browse_opts)

        a = Enum.find(found, &(&1.instance == "mdns-test-a"))
        b = Enum.find(found, &(&1.instance == "mdns-test-b"))

        assert %{port: ^port_a, txt: %{"clump_id" => "MdnsTestClump"}} = a
        assert %{port: ^port_b, txt: %{"clump_id" => "OtherClump"}} = b

        # Peer selection filters on clump and drops ourselves (port_a is
        # ours), leaving no other MdnsTestClump instance
        assert [] = Mdns.peers("MdnsTestClump", browse_opts ++ [port: port_a])

        # The other instance is visible to its own clump's members
        assert [%{port: ^port_b}] = Mdns.peers("OtherClump", browse_opts ++ [port: port_a])

        # An unknown clump finds nothing
        assert [] = Mdns.peers("NoSuchClump", browse_opts)

        MdnsLite.remove_mdns_service(:baby_MdnsTestClump)
        MdnsLite.remove_mdns_service(:baby_OtherClump)
      end)
    end
  end

  defp match_srv(dns_rr(type: t)), do: t == :srv
  defp match_a(dns_rr(type: t)), do: t == :a

  defp ephemeral_port do
    {:ok, listen} = :gen_tcp.listen(0, [:binary])
    {:ok, port} = :inet.port(listen)
    :gen_tcp.close(listen)
    port
  end
end
