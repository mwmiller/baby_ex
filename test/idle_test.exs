defmodule Baby.Connection.IdleTest do
  use ExUnit.Case, async: true

  alias Baby.Connection.Idle

  describe "new/0" do
    test "starts idle with randomized budgets above the synced budget" do
      idle = Idle.new()

      refute idle.synced
      assert idle.spins == 0
      assert idle.max_spins > 0
      assert idle.bootstrap_spins > idle.max_spins
      assert idle.handshake_spins > 0
      assert idle.bootstrap_spins > idle.handshake_spins
    end

    test "honours explicitly provided budgets" do
      idle = Idle.new(max_spins: 1000, bootstrap_spins: 5000, handshake_spins: 375)

      assert idle.max_spins == 1000
      assert idle.bootstrap_spins == 5000
      assert idle.handshake_spins == 375
    end

    test "reads budgets from Application config" do
      Application.put_env(:baby, :max_spins, 777)
      Application.put_env(:baby, :bootstrap_spins, 888)
      Application.put_env(:baby, :handshake_spins, 111)

      on_exit(fn ->
        Application.delete_env(:baby, :max_spins)
        Application.delete_env(:baby, :bootstrap_spins)
        Application.delete_env(:baby, :handshake_spins)
      end)

      idle = Idle.new()
      assert idle.max_spins == 777
      assert idle.bootstrap_spins == 888
      assert idle.handshake_spins == 111
    end

    test "per-connection opts take precedence over Application config" do
      Application.put_env(:baby, :max_spins, 777)

      on_exit(fn -> Application.delete_env(:baby, :max_spins) end)

      assert Idle.new(max_spins: 555).max_spins == 555
    end

    test "ignores invalid budget values and falls back to jittered defaults" do
      idle = Idle.new(max_spins: 0, bootstrap_spins: "lots", handshake_spins: :nope)
      assert idle.max_spins > 0
      assert idle.bootstrap_spins > 0
      assert idle.handshake_spins > 0
    end
  end

  describe "expired?/2" do
    test "a connection that has not completed a handshake is bounded by the tightest budget" do
      idle = %Idle{Idle.new() | spins: 2_999_999}
      assert Idle.expired?(idle, false)
    end

    test "an unsynced connection is bounded by the generous bootstrap budget" do
      idle = %Idle{Idle.new() | spins: 2_999_999}
      assert Idle.expired?(idle, true)
    end

    test "a synced connection is bounded by the tighter budget" do
      idle = %Idle{Idle.new() | synced: true, spins: 2_999_999}
      assert Idle.expired?(idle, true)
    end

    test "a fresh connection is not expired" do
      refute Idle.expired?(Idle.new(), false)
    end

    test "an unsynced connection over the synced budget is still within the bootstrap budget" do
      idle = %Idle{Idle.new() | synced: false, spins: idle_max_spins()}
      refute Idle.expired?(idle, true)
    end

    test "a handshaken connection over the handshake budget is still within the bootstrap budget" do
      idle = %Idle{Idle.new() | synced: false, spins: idle_handshake_spins()}
      refute Idle.expired?(idle, true)
    end
  end

  describe "poke/1" do
    test "any activity resets the counter without changing sync state" do
      idle = Idle.new() |> Idle.tick() |> Idle.tick() |> Idle.tick()
      assert idle.spins == 3

      idle = Idle.poke(idle)
      assert idle.spins == 0
      refute idle.synced
    end
  end

  describe "synced/1" do
    test "marks sync complete and restarts the clock" do
      idle = Idle.new() |> Idle.tick() |> Idle.synced()

      assert idle.synced
      assert idle.spins == 0
      refute Idle.expired?(idle, true)
    end
  end

  describe "describe/2" do
    test "includes the counter, budget and sync state" do
      assert Idle.describe(Idle.new(), false) =~ "synced: false"
    end

    test "a synced connection shows the synced budget" do
      idle = Idle.new() |> Idle.synced()
      assert Idle.describe(idle, true) =~ "synced: true"
      assert Idle.describe(idle, true) =~ Integer.to_string(idle.max_spins)
    end

    test "a handshaken but unsynced connection shows the bootstrap budget" do
      idle = Idle.new()
      assert Idle.describe(idle, true) =~ Integer.to_string(idle.bootstrap_spins)
    end

    test "a pre-handshake connection shows the handshake budget" do
      idle = Idle.new()
      assert Idle.describe(idle, false) =~ Integer.to_string(idle.handshake_spins)
    end
  end

  defp idle_max_spins do
    %Idle{Idle.new() | synced: true}.max_spins
  end

  defp idle_handshake_spins do
    %Idle{Idle.new() | synced: false}.handshake_spins
  end
end
