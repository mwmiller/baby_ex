defmodule Baby.MonitorTest do
  use ExUnit.Case, async: false
  import ExUnit.CaptureLog

  alias Baby.Monitor

  describe "cryout scheduling" do
    # Exercises the "meta cryout" wiring: an `mdns:` cryout browses instead of
    # dialing, finds nothing here (nothing is announced), and reschedules.
    # Two cycles must elapse inside the sleep window, proving the mdns branch
    # both survives its browse and re-arms itself.
    test "an mdns cryout cycles harmlessly when no peers are announced" do
      opts = %{cryouts: [], identity: nil, clump_id: "MonitorTestClump", port: 47_000}

      # start_link returns the monitor GenServer itself; sending a {:cryout, ...}
      # IS how the monitor schedules a round, so this runs the real mdns
      # discover path in place
      {:ok, monitor} = Baby.Monitor.start_link(opts)
      on_exit(fn -> Process.exit(monitor, :kill) end)

      send(monitor, {:cryout, mdns: [period: {1, :second}]})

      # Cycle 1: browse ~1s, reschedule +1s; cycle 2 starts ~1s, browses to ~2s
      Process.sleep(2400)

      assert Process.alive?(monitor)
    end
  end

  describe "per-monitor connection supervision" do
    test "each monitor owns a distinct DynamicSupervisor" do
      opts_a = %{cryouts: [], identity: nil, clump_id: "SupervisionA", port: 47_000}
      opts_b = %{cryouts: [], identity: nil, clump_id: "SupervisionB", port: 47_001}

      {:ok, a} = Baby.Monitor.start_link(opts_a)
      {:ok, b} = Baby.Monitor.start_link(opts_b)

      on_exit(fn ->
        Process.exit(a, :kill)
        Process.exit(b, :kill)
      end)

      %{conns_sup: sup_a} = :sys.get_state(a)
      %{conns_sup: sup_b} = :sys.get_state(b)

      assert is_pid(sup_a) and is_pid(sup_b)
      assert sup_a != sup_b
    end

    test "killing a monitor takes its connection supervisor down with it" do
      opts = %{cryouts: [], identity: nil, clump_id: "SupervisionC", port: 47_002}
      {:ok, monitor} = Baby.Monitor.start_link(opts)
      %{conns_sup: conns_sup} = :sys.get_state(monitor)

      ref = Process.monitor(conns_sup)

      # :kill is untrappable, so drop our own link first to avoid taking the
      # test process down with the monitor.  The cascaded :kill also makes OTP
      # log the supervisor's death, so fold that expected noise away.
      capture_log(fn ->
        Process.unlink(monitor)
        Process.exit(monitor, :kill)

        assert_receive {:DOWN, ^ref, :process, ^conns_sup, _}, 1_000
      end)
    end

    test "a graceful stop also takes the connection supervisor down" do
      opts = %{cryouts: [], identity: nil, clump_id: "SupervisionD", port: 47_003}
      {:ok, monitor} = Baby.Monitor.start_link(opts)
      %{conns_sup: conns_sup} = :sys.get_state(monitor)

      ref = Process.monitor(conns_sup)
      Process.unlink(monitor)
      GenServer.stop(monitor)

      assert_receive {:DOWN, ^ref, :process, ^conns_sup, _}, 1_000
    end
  end

  describe "next_delay_ms/1" do
    test "unconfigured mdns cryouts cycle about once a minute" do
      delay = Monitor.next_delay_ms(mdns: true)
      assert delay >= 40_000 and delay <= 80_000
    end

    test "explicit periods are honored exactly, wherever given" do
      assert Monitor.next_delay_ms(mdns: [period: {2, :second}]) == 2000
      assert Monitor.next_delay_ms(period: {3, :minute}, mdns: true) == 180_000
    end

    test "fixed-host cryouts keep the long default" do
      assert Monitor.next_delay_ms(host: "elsewhere", port: 8484) == 1_020_000
    end
  end
end
