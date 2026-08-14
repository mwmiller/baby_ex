defmodule Baby.Connection.Idle do
  @moduledoc """
  The idle-timeout state of a single `Baby.Connection`.

  A connection is considered idle when nothing has been sent or received for
  some number of outbox intervals (`spins`). Any real activity -- sending a
  packet or processing an inbound message -- resets the counter, so the budget
  measures *consecutive* silence rather than wall-clock time on a busy link.

  The budget is two-tiered:

    * while the initial replication sync has not completed (`synced: false`)
      the generous `bootstrap_spins` budget applies.  A peer may legitimately
      take a while to compute its WANT list, but a peer that stalls entirely
      must not be allowed to hang forever -- its registry entry would block
      every later cryout to that host.
    * once sync completes the tighter `max_spins` budget applies, so a caught
      up connection is dropped (and later re-established by the next cryout)
      rather than held open indefinitely.

  The whole unit is kept in the connection's state (`conn_info.idle`) so the
  live value can be inspected directly, e.g. `:sys.get_state(pid).idle`.
  """

  defstruct spins: 0,
            synced: false,
            max_spins: nil,
            bootstrap_spins: nil

  @type t :: %__MODULE__{
          spins: non_neg_integer(),
          synced: boolean(),
          max_spins: non_neg_integer(),
          bootstrap_spins: non_neg_integer()
        }

  @doc "A fresh idle timer with randomized budgets."
  @spec new() :: t()
  def new do
    %__MODULE__{
      max_spins: 1200 |> Primacy.primes_near(count: 5, dir: :below) |> Enum.random(),
      bootstrap_spins: 3000 |> Primacy.primes_near(count: 5, dir: :below) |> Enum.random()
    }
  end

  @doc """
  Record progress.  Any sent or received message resets the idle counter while
  leaving the sync state untouched.
  """
  @spec poke(t()) :: t()
  def poke(%__MODULE__{} = idle), do: %{idle | spins: 0}

  @doc "One idle interval has passed without any activity."
  @spec tick(t()) :: t()
  def tick(%__MODULE__{spins: spins} = idle), do: %{idle | spins: spins + 1}

  @doc "Mark the initial replication sync as complete and restart the clock."
  @spec synced(t()) :: t()
  def synced(%__MODULE__{} = idle), do: %{idle | synced: true, spins: 0}

  @doc "The connection has been idle for longer than its budget."
  @spec expired?(t()) :: boolean()
  def expired?(%__MODULE__{synced: true, spins: spins, max_spins: cap}), do: spins >= cap
  def expired?(%__MODULE__{synced: false, spins: spins, bootstrap_spins: cap}), do: spins >= cap

  @doc "A human readable summary for logging."
  @spec describe(t()) :: String.t()
  def describe(%__MODULE__{} = idle) do
    cap = if idle.synced, do: idle.max_spins, else: idle.bootstrap_spins
    "idle timeout (spins #{idle.spins}/#{cap}, synced: #{idle.synced})"
  end
end
