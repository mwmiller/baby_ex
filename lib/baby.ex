defmodule Baby do
  require Logger

  @moduledoc """
  Bushbaby Automated Bamboo Yields

  Reference `bushbaby` protocol node

  ### Configuration
  `spool_dir`: The path to the `Baobab` bamboo store spool
  `clumps`: List of per-clump keyword configurations
      - `id`: a binary `clump_id`
      - `controlling_identity`: a `Baobab.Identity` by which this peer will be known
      - `port`: an integer port to which to bind
      - `cryouts`: list of keyword configurations for periodic peer replication
          - `host`: peer host address
          - `port`: peer port
          - `period`: `{integer quantity, atom unit}` ({17, :minute})
  """

  @doc """
  Connect to a remote hort and port

  Keyword id_options relative to the configured `Baobab` store
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
