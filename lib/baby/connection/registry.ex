defmodule Baby.Connection.Registry do
  use GenServer

  @moduledoc """
  A process registry for `Baby.Connection`

  For maximum utility, name should be of the form:
  `{host, port}`
  where host is an internet address tuple and port is an integer
  """
  def start_link(_args) do
    GenServer.start_link(__MODULE__, nil, name: :conn_reg)
  end

  def init(_args) do
    {:ok, Map.new()}
  end

  # We conform to the API for using `:via` tuples, but statem is not a huge fan
  def whereis_name(conn_name) do
    GenServer.call(:conn_reg, {:whereis_name, conn_name})
  end

  def register_name(conn_name, pid) do
    GenServer.call(:conn_reg, {:register_name, conn_name, pid})
  end

  def unregister_name(conn_name) do
    GenServer.cast(:conn_reg, {:unregister_name, conn_name})
  end

  @doc """
  Send a message to a named connection
  """
  def send(conn_name, message) do
    case whereis_name(conn_name) do
      :none ->
        {:badarg, {conn_name, message}}

      pid ->
        pid_sender(pid, message)
    end
  end

  @doc """
  Send a message to all registered connections

  Can include a list of names or pids to exclude
  """
  def broadcast(message, except \\ []) do
    GenServer.call(:conn_reg, {:broadcast, message, except})
  end

  @doc """
  Return a list of all named connections
  """
  def active(), do: GenServer.call(:conn_reg, :active)

  @doc """
  Check if a named connection is active
  """
  def active?(conn_name), do: GenServer.call(:conn_reg, {:active, conn_name})

  def handle_call(:active, _from, state) do
    {:reply, state |> Map.keys() |> Enum.sort(), state}
  end

  def handle_call({:active, conn_name}, _from, state) do
    {:reply, Map.has_key?(state, conn_name), state}
  end

  def handle_call({:whereis_name, conn_name}, _from, state) do
    {:reply, Map.get(state, conn_name, :none), state}
  end

  def handle_call({:register_name, conn_name, pid}, _from, state) do
    case Map.get(state, conn_name) do
      nil ->
        # To allow cleanup on close
        Process.monitor(pid)
        {:reply, :yes, Map.put(state, conn_name, pid)}

      _ ->
        {:reply, :no, state}
    end
  end

  def handle_call({:broadcast, message, except}, _from, state) do
    res =
      state
      |> Map.drop(except)
      |> Map.values()
      |> Enum.reject(fn p -> p in except end)
      |> Enum.map(fn pid -> pid_sender(pid, message) end)

    {:reply, res, state}
  end

  # Meh
  defp pid_sender(pid, message) do
    Process.send(pid, message, [])
    pid
  end

  def handle_cast({:unregister_name, conn_name}, state) do
    {:noreply, Map.delete(state, conn_name)}
  end

  # Connection has gone down, we can clean up.
  def handle_info({:DOWN, _, :process, pid, _}, state) do
    {:noreply, state |> Enum.reject(fn {_, p} -> p == pid end) |> Enum.into(%{})}
  end
end
