defmodule Baby.MonitorTest do
  use ExUnit.Case, async: false

  # Exercises the "meta cryout" wiring: an `mdns:` cryout browses instead of
  # dialing, finds nothing here (nothing is announced), and reschedules.
  # Two cycles must elapse inside the sleep window, proving the mdns branch
  # both survives its browse and re-arms itself.
  test "an mdns cryout cycles harmlessly when no peers are announced" do
    opts = %{cryouts: [], identity: nil, clump_id: "MonitorTestClump", port: 47_000}

    {:ok, sup} = Baby.Monitor.start_link(opts)
    on_exit(fn -> Process.exit(sup, :kill) end)

    # start_link returns the wrapper supervisor; the loose GenServer is the
    # other proc_lib child we just spawned (its $ancestors include us)
    monitor =
      Process.list()
      |> Enum.find(fn pid ->
        pid != sup and spawned_by_us?(pid)
      end)

    assert is_pid(monitor)

    send(monitor, {:cryout, mdns: [period: {1, :second}]})

    # Cycle 1: browse ~1s, reschedule +1s; cycle 2 starts ~1s, browses to ~2s
    Process.sleep(2400)

    assert Process.alive?(monitor)
  end

  defp spawned_by_us?(pid) do
    case Process.info(pid, :dictionary) do
      {:dictionary, d} ->
        case List.keyfind(d, :"$ancestors", 0) do
          {:"$ancestors", ancestors} -> self() in ancestors
          _ -> false
        end

      _ ->
        false
    end
  end
end
