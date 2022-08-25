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
end
