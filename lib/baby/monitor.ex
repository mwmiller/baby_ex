defmodule Baby.Monitor do
  use GenServer
  require Logger

  def start_link(opts) when is_map(opts) do
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
  def handle_info({:cryout, opts}, state) do
    host = Keyword.get(opts, :host)
    Logger.info(["Crying out to ", host])
    Baby.connect(host, Keyword.get(opts, :port))
    # We get inherent jitter via the connection spin up
    next_start = to_ms(Keyword.get(opts, :period, {17, :minute}))

    Process.send_after(self(), {:cryout, opts}, next_start, [])
    {:noreply, state}
  end

  defp to_ms({amt, :millisecond}), do: amt
  defp to_ms({amt, :second}), do: to_ms({amt * 1000, :millisecond})
  defp to_ms({amt, :minute}), do: to_ms({amt * 60, :second})
  defp to_ms({amt, :hour}), do: to_ms({amt * 60, :minute})
  defp to_ms({amt, :day}), do: to_ms({amt * 24, :hour})
  defp to_ms({amt, :week}), do: to_ms({amt * 7, :day})
end