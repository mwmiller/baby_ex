defmodule Baby do
  @moduledoc """
  Documentation for `Baby`.
  """

  def connect(host, port) when is_binary(port), do: connect(host, String.to_integer(port))

  def connect(host, port) do
    Baby.Connection.start_link(
      host: host,
      port: port,
      identity: Application.get_env(:baby, :identity)
    )
  end
end
