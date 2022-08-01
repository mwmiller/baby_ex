defmodule BabyTest do
  use ExUnit.Case
  doctest Baby

  test "greets the world" do
    assert Baby.hello() == :world
  end
end
