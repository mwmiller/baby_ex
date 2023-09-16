defmodule Baby.Monitor do
  use GenServer
  alias Baby.Util
  require Logger

  @moduledoc """
  Cutely and unhelpfully named.
  A GenServer managing periodic `cryout` activity
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
  def handle_info({:cryout, opts}, %{identity: id, clump_id: clump} = state) do
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

    # We get inherent jitter via the connection spin up
    next_start = Util.period_to_ms(Keyword.get(opts, :period, {17, :minute}))

    Process.send_after(self(), {:cryout, opts}, next_start, [])
    {:noreply, state}
  end
end
