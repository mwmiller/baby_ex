defmodule Baby do
  alias Baby.Connection.Registry
  alias Baby.Util

  @moduledoc """
  Bushbaby Automated Bamboo Yields

  Reference `bushbaby` protocol node

  ### Configuration
  `spool_dir`: The path to the `Baobab` bamboo store spool
  `clumps`: List of per-clump keyword configurations
      - `id`: a binary `clump_id`
      - `controlling_identity`: a `Baobab.Identity` by which this peer will be known
      - `port`: an integer port to which to bind
      - `announce`: `true` (or `Baby.Mdns.announce/3` options) to make this
        clump visible to local network peers via mDNS
      - `cryouts`: list of keyword configurations for periodic peer replication
          - `host`: peer host address
          - `port`: peer port
          - `period`: `{integer quantity, atom unit}` ({17, :minute})
          - `mdns`: `true` (or keyword options incl. `period`) for a "meta
            cryout" that discovers and dials clump-mates seen on the local
            network instead of a fixed host
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
    do: host |> Util.host_to_ip() |> connect(port, id_options)

  def connect(host, port, id_options) do
    Baby.Connection.start_link(
      host: host,
      port: port,
      identity: Keyword.get(id_options, :identity),
      clump_id: Keyword.get(id_options, :clump_id)
    )
  end

  @doc """
  Determine if there is an active connection on a given `{host, port}`
  """
  def connected?({host, port}) when is_tuple(host), do: Registry.active?({host, port})

  def connected?({host, port}) do
    case Util.host_to_ip(host) do
      :error -> false
      ip -> connected?({ip, port})
    end
  end

  def connected?(_), do: false

  @doc """
  Determine if there is an active connection on an array of `{host, port}`

  Returns a map with the tuples as keys and a boolean result
  """
  def are_connected?(queries)

  def are_connected?(queries) do
    check_connections(queries, %{})
  end

  defp check_connections([], acc), do: acc

  defp check_connections([pair | rest], acc) do
    check_connections(rest, Map.put(acc, pair, connected?(pair)))
  end
end
