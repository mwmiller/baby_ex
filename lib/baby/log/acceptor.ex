defmodule Baby.Log.Acceptor do
  use GenServer

  @seg_bytes 20 * 1024 * 1024
  @moduledoc """
  Disk queue for incoming bamboo data

  The writing process, with its included verification, is much
  slower than the reading process.  This means that peers are
  sending data much faster than we can store it.

  As such, we have this level of indirection which gets the
  data out of memory (including process mailboxes) and on to
  disk.  We may even be able to recover old data upon restart
  """
  def start_link(_args) do
    GenServer.start_link(__MODULE__, nil, name: :log_acceptor)
  end

  def init(_args) do
    tmp_dir = Path.join([System.tmp_dir(), "replayq"])
    q = :replayq.open(%{dir: tmp_dir, seg_bytes: @seg_bytes})
    {:ok, %{queue: q}}
  end

  def add_job(data, conn_info) do
    GenServer.cast(:log_acceptor, {:add_it, data, conn_info})
  end

  def next_job() do
    GenServer.call(:log_acceptor, :get_it)
  end

  def ack_job(ackref) do
    GenServer.cast(:log_acceptor, {:ack_it, ackref})
  end

  def handle_cast({:add_it, data, conn_info}, %{queue: q} = state) do
    payload = :erlang.term_to_binary({conn_info, data})
    nq = :replayq.append(q, [payload])
    {:noreply, %{state | queue: nq}}
  end

  def handle_cast({:ack_it, ackref}, %{queue: q} = state) do
    :ok = :replayq.ack(q, ackref)
    {:noreply, state}
  end

  def handle_call(:get_it, _from, %{queue: q} = state) do
    # It might be worth considering popping a few at a time
    # in the future
    {nq, reply} =
      case :replayq.pop(q, %{count_limit: 1}) do
        {nq, :nothing_to_ack, _} -> {nq, :none}
        {nq, ackref, [payload]} -> {nq, {ackref, :erlang.binary_to_term(payload)}}
      end

    {:reply, reply, %{state | queue: nq}}
  end
end
