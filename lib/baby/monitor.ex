defmodule Baby.Monitor do
  use GenServer

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
    again = Keyword.get(opts, :period_ms, 1_022_677)
    Baby.connect(Keyword.get(opts, :host), Keyword.get(opts, :port))

    Process.send_after(self(), {:cryout, opts}, again, [])
    {:noreply, state}
  end
end
