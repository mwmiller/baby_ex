defmodule Baby.Log.Writer do
  use GenServer
  alias Baby.Util
  alias Baby.Log.Acceptor

  @rest 911
  @moduledoc """
  Stand alone writer for the `Baobab` log store
  """
  def start_link(_args) do
    GenServer.start_link(__MODULE__, nil, name: :log_writer)
  end

  def init(_args) do
    {:ok, %{}, {:continue, :first_loop}}
  end

  def handle_continue(:first_loop, state), do: acceptor_loop(:run, state)

  def handle_info(:wakey, state), do: acceptor_loop(:run, state)

  defp acceptor_loop(:stop, state) do
    Process.send_after(:log_writer, :wakey, @rest, [])
    {:noreply, state}
  end

  defp acceptor_loop(:run, state) do
    next =
      case Acceptor.next_job() do
        :none ->
          :stop

        {ackref, {conn_info, data}} ->
          data
          |> Baobab.Interchange.import_binaries(clump_id: conn_info.clump_id, replace: false)
          |> report(conn_info, MapSet.new())

          Acceptor.ack_job(ackref)
          :run
      end

    acceptor_loop(next, state)
  end

  defp report([], _, _), do: :ok

  defp report([{:error, reason} | rest], conn_info, added) do
    Util.connection_log(conn_info, :in, "import error: " <> reason, :warning)
    report(rest, conn_info, added)
  end

  # This used to have reporting to other processes that there was
  # new data. It was just proof of concept, but the accumulator
  # remains for now.
  defp report([%Baobab.Entry{author: a, log_id: l} | rest], conn_info, added) do
    report(rest, conn_info, MapSet.put(added, {Baobab.Identity.as_base62(a), l}))
  end

  defp report([_ | rest], conn_info, added), do: report(rest, conn_info, added)
end
