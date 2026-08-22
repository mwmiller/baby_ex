defmodule Baby.Monitor do
  use GenServer
  alias Baby.Connection.Registry
  alias Baby.Util
  require Logger

  @moduledoc """
  Cutely and unhelpfully named.
  A GenServer managing periodic `cryout` activity

  A cryout configuration naming an `mdns` key is a "meta cryout":
  rather than dialing a fixed `{host, port}`, we periodically browse
  the local network for announced peers of our clump (see
  `Baby.Mdns`) and connect to whatever we see there.
  """

  def start_link(opts) when is_map(opts) do
    children = [
      {DynamicSupervisor, strategy: :one_for_one, name: Baby.Monitor.DynamicSupervisor}
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
    GenServer.start_link(__MODULE__, opts)
  end

  @impl true
  def init(%{cryouts: cryouts} = state) do
    for peer <- cryouts do
      Process.send_after(self(), {:cryout, peer}, :rand.uniform(3000), [])
    end

    {:ok, state}
  end

  @impl true
  def handle_info({:cryout, opts}, %{identity: id, clump_id: clump, port: our_port} = state) do
    if Keyword.has_key?(opts, :mdns) do
      discover(clump, id, our_port)
    else
      host = Keyword.get(opts, :host)
      Logger.info(["Crying out to ", host])

      DynamicSupervisor.start_child(
        Baby.Monitor.DynamicSupervisor,
        {Baby.Connection,
         [
           host: Util.host_to_ip(host),
           port: Keyword.get(opts, :port),
           identity: id,
           clump_id: clump
         ]}
      )
    end

    # We get inherent jitter via the connection spin up
    next_start = Util.period_to_ms(period(opts))

    Process.send_after(self(), {:cryout, opts}, next_start, [])
    {:noreply, state}
  end

  # Connect to every discovered clump-mate that we aren't already
  # talking to.  The connection registry keeps repeat browses from
  # stacking duplicate connections.
  defp discover(clump_id, identity, our_port) do
    found = Baby.Mdns.peers(clump_id, port: our_port)

    for %{ip: ip, port: port} <- found,
        not Registry.active?({ip, port}) do
      Logger.info(["Crying out to discovered ", :inet.ntoa(ip), ":", Integer.to_string(port)])

      DynamicSupervisor.start_child(
        Baby.Monitor.DynamicSupervisor,
        {Baby.Connection, [host: ip, port: port, identity: identity, clump_id: clump_id]}
      )
    end
  end

  # A cryout's period comes from its own top level, from within a meta
  # cryout's options, or falls back to the default
  defp period(opts),
    do: Keyword.get(opts, :period) || meta_period(Keyword.get(opts, :mdns)) || {17, :minute}

  defp meta_period(nil), do: nil
  defp meta_period(meta) when is_list(meta), do: Keyword.get(meta, :period)
  defp meta_period(_), do: nil
end
