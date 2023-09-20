defmodule Baby.LogWriter do
  use GenServer
  alias Baby.Util
  alias Baby.Connection.Registry

  @moduledoc """
  Stand alone writer for the `Baobab` log store
  """
  def start_link(_args) do
    GenServer.start_link(__MODULE__, nil, name: :log_writer)
  end

  def init(_args) do
    {:ok, %{}}
  end

  @doc """
  Request import of raw Bamboo binaries

  Logs errors as `:warning`
  """
  def import_their(data, conn_info) do
    GenServer.cast(:log_writer, {:log_it, data, conn_info})
    conn_info
  end

  def handle_cast({:log_it, stuff, conn_info}, state) do
    stuff
    |> Baobab.Interchange.import_binaries(clump_id: conn_info.clump_id, replace: false)
    |> report(conn_info, MapSet.new())

    {:noreply, state}
  end

  defp report([], %{clump_id: clump_id, pid: pid}, added) do
    case MapSet.to_list(added) do
      [] ->
        :ok

      new ->
        Registry.broadcast(
          {:added, clump_id,
           Enum.map(new, fn {a, l} ->
             {a, l, Baobab.max_seqnum(a, log_id: l, clump_id: clump_id)}
           end)},
          [pid]
        )
    end
  end

  defp report([{:error, reason} | rest], conn_info, added) do
    Util.connection_log(conn_info, :in, "import error: " <> reason, :warning)
    report(rest, conn_info, added)
  end

  defp report([%Baobab.Entry{author: a, log_id: l} | rest], conn_info, added) do
    report(rest, conn_info, MapSet.put(added, {Baobab.Identity.as_base62(a), l}))
  end

  defp report([_ | rest], conn_info, added), do: report(rest, conn_info, added)
end
