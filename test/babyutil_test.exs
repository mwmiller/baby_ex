defmodule BabyUtilTest do
  use ExUnit.Case
  doctest Baby.Util
  alias Baby.Util

  describe "period_to_ms/1" do
    test "converts common period units to milliseconds" do
      assert 1 == Util.period_to_ms({1, :millisecond})
      assert 1000 == Util.period_to_ms({1, :second})
      assert 1_380_000 == Util.period_to_ms({23, :minute})
      assert 18_000_000 == Util.period_to_ms({5, :hour})
      assert 950_400_000 == Util.period_to_ms({11, :day})
      assert 4_233_600_000 == Util.period_to_ms({7, :week})
    end

    test "rejects unknown period units" do
      assert :error == Util.period_to_ms({2, :month})
    end
  end

  describe "range_points/1" do
    test "returns empty ranges for an empty list" do
      assert [] == Util.range_points([])
    end

    test "wraps a single point as a one-point range" do
      assert [{1, 1}] == Util.range_points([1])
    end

    test "compresses consecutive points into a single range" do
      assert [{1, 20}] == Util.range_points(1..20)
    end

    test "splits non-consecutive points into multiple ranges" do
      assert [{1, 5}, {7, 10}] == Util.range_points([1, 2, 3, 4, 5, 7, 8, 9, 10])
    end
  end

  describe "host_to_ip/1" do
    test "resolves a hostname to an IP tuple" do
      # This is about the only one on which I can count across environments
      assert {127, 0, 0, 1} == Util.host_to_ip("localhost")
    end

    test "rejects non-binary inputs" do
      assert :error == Util.host_to_ip({127, 0, 0, 1})
    end
  end
end
