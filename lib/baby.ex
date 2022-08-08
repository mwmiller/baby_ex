defmodule Baby do
  @moduledoc """
  Bushbaby Automated Bamboo Yields
  """

  @doc """
  Connect to a remote hort and port with a given identity
  The confiured identity is the default
  """
  def connect(host, port), do: connect(host, port, Application.get_env(:baby, :identity))

  def connect(host, port, identity) when is_binary(port),
    do: connect(host, String.to_integer(port), identity)

  def connect(host, port, identity) when is_binary(host) do
    case :inet.gethostbyname(to_charlist(host)) do
      {:ok, {:hostent, _, _, _, _, [addr | _more]}} -> connect(addr, port, identity)
      _ -> :error
    end
  end

  def connect(host, port, identity) do
    Baby.Connection.start_link(
      host: host,
      port: port,
      identity: identity
    )
  end
end
