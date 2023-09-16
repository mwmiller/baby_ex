defmodule BabyUtilTest do
  use ExUnit.Case
  doctest Baby.Util
  alias Baby.Util

  test "period_to_ms" do
    assert 1 == Util.period_to_ms({1, :millisecond})
    assert 1000 == Util.period_to_ms({1, :second})
    assert 1_380_000 == Util.period_to_ms({23, :minute})
    assert 18_000_000 == Util.period_to_ms({5, :hour})
    assert 950_400_000 == Util.period_to_ms({11, :day})
    assert 4_233_600_000 == Util.period_to_ms({7, :week})
    assert :error == Util.period_to_ms({2, :month})
  end

  test "range_points" do
    assert [] == Util.range_points([])
    assert [{1, 1}] == Util.range_points([1])
    assert [{1, 20}] == Util.range_points(1..20)
    assert [{1, 5}, {7, 10}] == Util.range_points([1, 2, 3, 4, 5, 7, 8, 9, 10])
  end

  test "host_to_ip" do
    # This is about the only one on which I can count across envrionments
    assert {127, 0, 0, 1} == Util.host_to_ip("localhost")
    assert :error == Util.host_to_ip({127, 0, 0, 1})
  end
end
