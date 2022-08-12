defmodule Baby do
  @moduledoc """
  Bushbaby Automated Bamboo Yields
  """

  @doc """
  Connect to a remote hort and port

  Keyword id_options may override the standard config:
  - clump_id
  - identity
  """
  def connect(host, port, id_options \\ [])

  def connect(host, port, id_options) when is_binary(port),
    do: connect(host, String.to_integer(port), id_options)

  def connect(host, port, id_options) when is_binary(host) do
    where = to_charlist(host)

    case :inet.gethostbyname(where) do
      {:ok, {:hostent, _, _, _, _, [addr | _more]}} ->
        connect(addr, port, id_options)

      _ ->
        case :inet.parse_ipv6_address(where) do
          {:ok, addr} -> connect(addr, port, id_options)
          _ -> :error
        end
    end
  end

  def connect(host, port, id_options) do
    Baby.Connection.start_link(
      host: host,
      port: port,
      identity: Keyword.get(id_options, :identity, Application.get_env(:baby, :identity)),
      clump_id: Keyword.get(id_options, :clump_id, Application.get_env(:baby, :clump_id))
    )
  end
end
