defmodule Baby.ApplicationTest do
  use ExUnit.Case, async: false
  import ExUnit.CaptureLog

  describe "starting a clump listener via Ranch" do
    setup do
      port = ephemeral_port()

      [ref] =
        start_app([
          [
            id: "ApplicationTestClump",
            controlling_identity: "application_test_identity",
            port: port
          ]
        ])

      %{port: port, ref: ref}
    end

    test "accepts connections on the configured port", %{port: port} do
      # The connection handshake logs a HELLO; read the greeting inside the
      # capture so it both proves the connection fully registered (rather than
      # racing teardown) and stays out of the test output
      capture_log(fn ->
        {:ok, sock} = :gen_tcp.connect({127, 0, 0, 1}, port, [:binary, active: false], 1000)

        # The connected socket must report it is talking to the *configured*
        # port, not to whatever the OS gave a misconfigured listener
        assert {:ok, {{127, 0, 0, 1}, ^port}} = :inet.peername(sock)

        # The server greets with a HELLO on enter; reading it proves the
        # connection fully finished init (registered with the Registry) rather
        # than still racing teardown
        assert {:ok, _hello} = :gen_tcp.recv(sock, 0, 1_000)

        :gen_tcp.close(sock)
      end)
    end

    test "reports the listener is bound to the configured port", %{port: port, ref: ref} do
      # ranch stores {ip, port}; the ip half is OS-dependent (wildcard on some
      # systems), so assert on the port half
      assert {_, ^port} = :ranch.get_addr(ref)
    end
  end

  describe "multiple configured clumps" do
    setup do
      port_a = ephemeral_port()
      port_b = ephemeral_port()

      clump_a = [id: "MultiClumpA", controlling_identity: "multi_identity_a", port: port_a]
      clump_b = [id: "MultiClumpB", controlling_identity: "multi_identity_b", port: port_b]

      refs = start_app([clump_a, clump_b])

      %{port_a: port_a, port_b: port_b, refs: refs}
    end

    test "each clump's listener binds to its own configured port", %{
      port_a: port_a,
      port_b: port_b,
      refs: [ref_a, ref_b]
    } do
      assert port_a != port_b
      assert {_, ^port_a} = :ranch.get_addr(ref_a)
      assert {_, ^port_b} = :ranch.get_addr(ref_b)
    end

    test "each clump's monitor owns its own connection supervisor" do
      children = Supervisor.which_children(Baby.Supervisor)

      %{conns_sup: sup_a} = :sys.get_state(monitor_child(children, :MultiClumpA))
      %{conns_sup: sup_b} = :sys.get_state(monitor_child(children, :MultiClumpB))

      assert is_pid(sup_a) and is_pid(sup_b)
      assert sup_a != sup_b
    end
  end

  describe "peer rendezvous via a meta cryout" do
    setup do
      app_port = ephemeral_port()
      peer_port = ephemeral_port()

      start_app([
        [
          id: "RendezvousClump",
          controlling_identity: "rendezvous_identity",
          port: app_port,
          # A "meta cryout": browse the local network for clump-mates
          # instead of dialing a fixed host
          cryouts: [[mdns: [period: {1, :second}]]]
        ]
      ])

      # Remove our announcement before the app tears down, so later test
      # modules do not discover a stale rendezvous peer
      on_exit(fn -> MdnsLite.remove_mdns_service(:baby_RendezvousClump) end)

      %{peer_port: peer_port}
    end

    test "a meta cryout discovers an announced clump-mate and dials it", %{peer_port: peer_port} do
      # A pretend clump-mate: announced via mDNS with a real listening socket
      # behind it so the discovered connection has somewhere to land
      {:ok, listen} = :gen_tcp.listen(peer_port, [:binary, active: false, ip: {0, 0, 0, 0}])
      on_exit(fn -> :gen_tcp.close(listen) end)

      capture_log(fn ->
        :ok = Baby.Mdns.announce("RendezvousClump", peer_port, instance: "rendezvous-peer")

        # The monitor's meta cryout fires within ~3s of start and re-arms every
        # second; each cycle browses for clump-mates and dials what it finds.
        # Accepting proves the discovery actually turned into a connection.
        assert {:ok, accepted} = :gen_tcp.accept(listen, 10_000)

        # And it really is our protocol: the dialing peer greets with a HELLO
        assert {:ok, _hello} = :gen_tcp.recv(accepted, 0, 1_000)
      end)
    end
  end

  describe "failing to bind" do
    setup do
      {:ok, _} = Application.ensure_all_started(:baobab)

      spool_dir =
        Path.join(System.tmp_dir!(), "baby_test_spool_#{System.unique_integer([:positive])}")

      File.mkdir_p!(spool_dir)
      original_spool_dir = Application.get_env(:baobab, :spool_dir)

      on_exit(fn ->
        if original_spool_dir do
          Application.put_env(:baobab, :spool_dir, original_spool_dir)
        else
          Application.delete_env(:baobab, :spool_dir)
        end

        File.rm_rf!(spool_dir)
      end)

      %{spool_dir: spool_dir}
    end

    test "a clump whose port is taken fails the whole start", %{spool_dir: spool_dir} do
      port = ephemeral_port()
      {:ok, occupier} = :gen_tcp.listen(port, [:binary, ip: {0, 0, 0, 0}])
      on_exit(fn -> :gen_tcp.close(occupier) end)

      config = [
        spool_dir: spool_dir,
        clumps: [[id: "BindFailClump", controlling_identity: "bind_fail_identity", port: port]]
      ]

      # ranch logs the listen failure, so capture the expected noise
      {{:error, _reason}, _log} = with_log(fn -> Baby.Application.start(:normal, config) end)
    end

    test "a later clump's bind failure rolls back earlier listeners", %{spool_dir: spool_dir} do
      port_a = ephemeral_port()
      port_b = ephemeral_port()
      {:ok, occupier} = :gen_tcp.listen(port_b, [:binary, ip: {0, 0, 0, 0}])
      on_exit(fn -> :gen_tcp.close(occupier) end)

      clump_a = [id: "RollbackClumpA", controlling_identity: "rollback_a", port: port_a]
      clump_b = [id: "RollbackClumpB", controlling_identity: "rollback_b", port: port_b]

      config = [spool_dir: spool_dir, clumps: [clump_a, clump_b]]

      {{:error, _reason}, _log} = with_log(fn -> Baby.Application.start(:normal, config) end)

      # Clump A bound fine but the app failed on clump B; A's listener must
      # have been rolled back, so nothing is listening on its port
      assert {:error, :econnrefused} = :gen_tcp.connect({127, 0, 0, 1}, port_a, [], 500)
    end

    test "the same config succeeds once the port is freed", %{spool_dir: spool_dir} do
      port = ephemeral_port()
      {:ok, occupier} = :gen_tcp.listen(port, [:binary, ip: {0, 0, 0, 0}])

      clump = [id: "RetryClump", controlling_identity: "retry_identity", port: port]
      config = [spool_dir: spool_dir, clumps: [clump]]

      {{:error, _reason}, _log} = with_log(fn -> Baby.Application.start(:normal, config) end)

      :gen_tcp.close(occupier)

      # If the failed start had leaked the listener or its ranch ref, this
      # retry would fail on an already-bound port
      {{:ok, sup}, _log} = with_log(fn -> Baby.Application.start(:normal, config) end)

      on_exit(fn ->
        :ranch.stop_listener(:baby_RetryClump)
        Process.exit(sup, :kill)
      end)
    end
  end

  # Boot the whole application for a set of clump configs, registering cleanup
  # (listeners stopped, supervisor killed, baobab spool restored) on exit.
  # Returns the ranch listener refs, one per clump.
  defp start_app(clumps) do
    {:ok, _} = Application.ensure_all_started(:baobab)

    spool_dir =
      Path.join(System.tmp_dir!(), "baby_test_spool_#{System.unique_integer([:positive])}")

    File.mkdir_p!(spool_dir)
    original_spool_dir = Application.get_env(:baobab, :spool_dir)

    config = [spool_dir: spool_dir, clumps: clumps]

    # Starting Baobab and the ranch listeners can emit start-up logging, so
    # capture it rather than leak it into the test runner's output
    {{:ok, sup}, _log} = with_log(fn -> Baby.Application.start(:normal, config) end)

    refs = for clump <- clumps, do: String.to_atom("baby_" <> Keyword.fetch!(clump, :id))

    on_exit(fn ->
      Enum.each(refs, &:ranch.stop_listener/1)
      Process.exit(sup, :kill)

      # Baby.Application.start put_env'd our tmp spool; restore the prior
      # value so later test modules see an intact spool directory
      if original_spool_dir do
        Application.put_env(:baobab, :spool_dir, original_spool_dir)
      else
        Application.delete_env(:baobab, :spool_dir)
      end

      File.rm_rf!(spool_dir)
    end)

    refs
  end

  defp monitor_child(children, clump_id) do
    {^clump_id, pid, _, _} =
      Enum.find(children, fn {id, _, _, _} -> id == clump_id end)

    assert is_pid(pid)
    pid
  end

  defp ephemeral_port do
    {:ok, listen} = :gen_tcp.listen(0, [:binary, ip: {127, 0, 0, 1}])
    {:ok, port} = :inet.port(listen)
    :ok = :gen_tcp.close(listen)
    port
  end
end
