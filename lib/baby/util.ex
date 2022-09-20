defmodule Baby.Util do
  require Logger

  defp arrow(:in), do: "⇒"
  defp arrow(:out), do: "⇐"
  defp arrow(:both), do: "⇔"

  def connection_log(conn_info, dir, msg, level \\ :debug)

  def connection_log(ci, d, msg, level) when is_atom(msg),
    do: connection_log(ci, d, Atom.to_string(msg), level)

  def connection_log(conn_info, dir, msg, level) do
    Logger.log(level, Enum.join([tilde_peer(conn_info), arrow(dir), msg], " "))
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
end
