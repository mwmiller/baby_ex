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

  An unconfigured meta cryout cycles about once a minute -- each
  interval a random prime number of seconds near 60 -- giving jitter
  so that many peers do not browse in lockstep; an explicit `period`
  is honored exactly.
  """

  # Fixed-host peers are relatively static, so an infrequent revisit
  # suffices; discovered peers come and go, so meta cryouts check in
  # far more often
  @default_period {17, :minute}
  @mdns_nominal 60

  def start_link(opts) when is_map(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  @impl true
  def init(%{cryouts: cryouts} = state) do
    # This monitor owns the supervisor its cryouts spawn connections under.
    # It is deliberately unnamed and per-monitor, so any number of clumps can
    # each get their own; the link makes the batch of spawned peers die with
    # us (see terminate/2 for the graceful half of that pairing).
    {:ok, conns_sup} = DynamicSupervisor.start_link(strategy: :one_for_one)

    for peer <- cryouts do
      Process.send_after(self(), {:cryout, peer}, :rand.uniform(3000), [])
    end

    {:ok, Map.put(state, :conns_sup, conns_sup)}
  end

  @impl true
  def terminate(_reason, %{conns_sup: conns_sup}) do
    # The link already covers the crash path; this covers a graceful stop.
    # Guard in case the supervisor already exhausted its own restarts and
    # died, so terminate cannot raise on a dead pid.
    if Process.alive?(conns_sup) do
      Supervisor.stop(conns_sup, :normal, 5000)
    end

    :ok
  end

  def terminate(_reason, _state), do: :ok

  @impl true
  def handle_info(
        {:cryout, opts},
        %{identity: id, clump_id: clump, port: our_port, conns_sup: conns_sup} = state
      ) do
    if Keyword.has_key?(opts, :mdns) do
      discover(conns_sup, clump, id, our_port)
    else
      host = Keyword.get(opts, :host)
      Logger.info(["Crying out to ", host])

      DynamicSupervisor.start_child(
        conns_sup,
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
    next_start = next_delay_ms(opts)

    Process.send_after(self(), {:cryout, opts}, next_start)
    {:noreply, state}
  end

  # Connect to every discovered clump-mate that we aren't already
  # talking to.  The connection registry keeps repeat browses from
  # stacking duplicate connections.
  defp discover(conns_sup, clump_id, identity, our_port) do
    found = Baby.Mdns.peers(clump_id, port: our_port)

    for %{ip: ip, port: port} <- found,
        not Registry.active?({ip, port}) do
      Logger.info(["Crying out to discovered ", :inet.ntoa(ip), ":", Integer.to_string(port)])

      DynamicSupervisor.start_child(
        conns_sup,
        {Baby.Connection, [host: ip, port: port, identity: identity, clump_id: clump_id]}
      )
    end
  end

  # Milliseconds until this cryout should fire again: an explicitly
  # configured `period` (at the cryout's top level or within its
  # `mdns` options) wins; otherwise fixed-host cryouts fall back to
  # their long default and meta cryouts to about a minute of prime-
  # numbered seconds
  @doc false
  def next_delay_ms(opts) do
    case Keyword.get(opts, :period) || meta_period(Keyword.get(opts, :mdns)) do
      nil -> fallback_delay_ms(opts)
      period -> Util.period_to_ms(period)
    end
  end

  defp fallback_delay_ms(opts) do
    if Keyword.has_key?(opts, :mdns) do
      @mdns_nominal
      |> Primacy.primes_near(count: 10)
      |> Enum.random()
      |> Kernel.*(1000)
    else
      Util.period_to_ms(@default_period)
    end
  end

  defp meta_period(nil), do: nil
  defp meta_period(meta) when is_list(meta), do: Keyword.get(meta, :period)
  defp meta_period(_), do: nil
end
