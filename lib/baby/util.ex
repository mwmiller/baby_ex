defmodule Baby.Util do
  require Logger

  @moduledoc """
  Utility functions for use across the codebase
  """

  defp arrow(:in), do: "⇒"
  defp arrow(:out), do: "⇐"
  defp arrow(:both), do: "⇔"

  @doc """
  Standardised connection activity logging for the supplied state
    `dir`: `:in`, `:out`, `:both`
    `msg`: a protocol message type atom
    `level`: log lvel atom (default: `:debug`)
  """
  def connection_log(conn_info, dir, msg, level \\ :debug)

  def connection_log(ci, d, msg, level) when is_atom(msg),
    do: connection_log(ci, d, Atom.to_string(msg), level)

  def connection_log(conn_info, dir, msg, level) do
    Logger.log(level, Enum.join([tilde_peer(conn_info), arrow(dir), msg], " "))
  end

  @doc """
  Logging of fatal errors takes connection info and the error

  Returns `:error`
  """
  def log_fatal(conn_info, error) do
    Logger.log(:error, Enum.join([tilde_peer(conn_info), error], " "))
    :error
  end

  defp tilde_peer(conn_info) do
    case Map.fetch(conn_info, :short_peer) do
      {:ok, them} -> them
      :error -> "~unknown"
    end
  end

  @doc """
  Returns tuples of the endpoints of the widest continuous ranges
  in a list of integers

      iex> Baby.Util.range_points([8, 2, 5, 4, 3, 10, 11, 16, 17])
      [{2, 5}, {8, 8}, {10, 11}, {16, 17}]

      iex> Baby.Util.range_points([127])
      [{127, 127}]

      iex> Baby.Util.range_points([])
      []
  """

  # Since the working part requires so many parameters and a
  # distinct sorted list, we do some likely extra setup work here
  # This would probably suck if the lists were too large

  def range_points(list) do
    list |> Enum.sort() |> Enum.uniq() |> range_points(nil, nil, [])
  end

  defp range_points([], nil, nil, acc), do: Enum.reverse(acc)

  defp range_points([], final, first, acc),
    do: range_points([], nil, nil, [{first, final} | acc])

  defp range_points([n | rest], curr, first, acc) do
    cond do
      curr == nil -> range_points(rest, n, n, acc)
      curr == n - 1 -> range_points(rest, n, first, acc)
      true -> range_points(rest, n, n, [{first, curr} | acc])
    end
  end

  @doc """
  Convert a `{count, unit}` period into a number of milliseconds

  ## Examples

       iex> Baby.Util.period_to_ms({1, :hour})
       3_600_000


       iex> Baby.Util.period_to_ms({17, :minute})
       1_020_000

  """
  @spec period_to_ms({integer, atom}) :: integer | :error
  def period_to_ms(period)
  def period_to_ms({amt, :millisecond}), do: amt
  def period_to_ms({amt, :second}), do: period_to_ms({amt * 1000, :millisecond})
  def period_to_ms({amt, :minute}), do: period_to_ms({amt * 60, :second})
  def period_to_ms({amt, :hour}), do: period_to_ms({amt * 60, :minute})
  def period_to_ms({amt, :day}), do: period_to_ms({amt * 24, :hour})
  def period_to_ms({amt, :week}), do: period_to_ms({amt * 7, :day})
  def period_to_ms(_), do: :error
end
