defmodule Baby do
  require Logger

  @moduledoc """
  Bushbaby Automated Bamboo Yields
  """

  @doc """
  Connect to a remote hort and port

  Keyword id_options:
  - clump_id
  - identity
  """
  def connect(host, port, id_options \\ [])

  def connect(host, port, id_options) when is_binary(port),
    do: connect(host, String.to_integer(port), id_options)

  def connect(host, port, id_options) when is_binary(host),
    do: connect(host_to_ip(host), port, id_options)

  def connect(host, port, id_options) do
    Baby.Connection.start_link(
      host: host,
      port: port,
      identity: Keyword.get(id_options, :identity),
      clump_id: Keyword.get(id_options, :clump_id)
    )
  end

  @doc false
  def host_to_ip(host) do
    where = to_charlist(host)

    case :inet.gethostbyname(where) do
      {:ok, {:hostent, _, _, _, _, [addr | _more]}} ->
        addr

      _ ->
        case :inet.parse_ipv6_address(where) do
          {:ok, addr} -> addr
          _ -> :error
        end
    end
  end
end
