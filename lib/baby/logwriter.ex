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
    {:ok, MapSet.new()}
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
    {:noreply,
     stuff
     |> Baobab.Interchange.import_binaries(clump_id: conn_info.clump_id, replace: false)
     |> report(conn_info, {[], state})}
  end

  defp report([], %{clump_id: clump_id, pid: pid}, {ls, gs}) do
    case ls do
      [] -> :ok
      new -> Registry.broadcast({:added, clump_id, Enum.reverse(new)}, [pid])
    end

    gs
  end

  defp report([{:error, reason} | rest], conn_info, acc) do
    Util.connection_log(conn_info, :in, "import error: " <> reason, :warning)
    report(rest, conn_info, acc)
  end

  defp report([%Baobab.Entry{author: a, log_id: l, seqnum: s} | rest], conn_info, {ls, gs} = acc) do
    new = {Baobab.Identity.as_base62(a), l, s}

    acc =
      case MapSet.member?(gs, new) do
        true -> acc
        false -> {[new | ls], MapSet.put(gs, new)}
      end

    report(rest, conn_info, acc)
  end

  defp report([_ | rest], conn_info, acc), do: report(rest, conn_info, acc)
end
