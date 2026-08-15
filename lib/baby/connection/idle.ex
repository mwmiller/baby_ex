defmodule Baby.Connection.Idle do
  @moduledoc """
  The idle-timeout state of a single `Baby.Connection`.

  A connection is considered idle when nothing has been sent or received for
  some number of outbox intervals (`spins`). Any real activity -- sending a
  packet or processing an inbound message -- resets the counter, so the budget
  measures *consecutive* silence rather than wall-clock time on a busy link.

  The budget is three-tiered, shrinking as the connection earns trust:

    * before a valid `HELLO` has been exchanged (`short_peer` is unset) the
      tight `handshake_spins` budget applies, so an anonymous flood that never
      completes a handshake is dropped quickly and cannot pin the listener's
      connection slots.
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
            bootstrap_spins: nil,
            handshake_spins: nil

  @type t :: %__MODULE__{
          spins: non_neg_integer(),
          synced: boolean(),
          max_spins: non_neg_integer(),
          bootstrap_spins: non_neg_integer(),
          handshake_spins: non_neg_integer()
        }

  @doc """
  Create a fresh idle timer.

  All budgets are measured in outbox intervals (i.e. `spins`); multiply by
  the connection's `outrate` for wall-clock time.  By default each is a random
  prime near a nominal value, giving jitter so that many connections do not
  drop in lockstep.

  Consumers may pin them via Application config, which is read as the source
  of truth:

      config :baby, max_spins: 1000, bootstrap_spins: 5000

  or per-connection, which takes precedence over the Application config:

    * `:handshake_spins` - budget before a valid `HELLO` has been received.
      Defaults to a random prime near 375 (~30s at the default outrate).
    * `:max_spins` - budget once the initial sync has completed.
      Defaults to a random prime near 1200.
    * `:bootstrap_spins` - budget while the initial sync is still in
      progress.  Defaults to a random prime near 3000.

  Example: `Idle.new(max_spins: 1000, bootstrap_spins: 5000)`
  """
  @spec new(keyword()) :: t()
  def new(opts \\ []) do
    %__MODULE__{
      max_spins: spin_budget(opts, :max_spins, 1200),
      bootstrap_spins: spin_budget(opts, :bootstrap_spins, 3000),
      handshake_spins: spin_budget(opts, :handshake_spins, 375)
    }
  end

  defp spin_budget(opts, key, near) do
    case Keyword.get(opts, key) || Application.get_env(:baby, key) do
      n when is_integer(n) and n > 0 ->
        n

      _ ->
        near |> Primacy.primes_near(count: 5, dir: :below) |> Enum.random()
    end
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

  @doc """
  The connection has been idle for longer than its budget.

  `handshaken?` indicates whether a valid `HELLO` has been received, which
  selects between the pre-handshake and mid-bootstrap budgets for a connection
  whose initial sync has not yet completed.

  This budget selection is mirrored by the idle-disconnect clauses in
  `Baby.Connection`; keep the two in step when adding tiers.
  """
  @spec expired?(t(), boolean()) :: boolean()
  def expired?(%__MODULE__{synced: true, spins: spins, max_spins: cap}, _handshaken?),
    do: spins >= cap

  def expired?(%__MODULE__{synced: false, spins: spins, handshake_spins: cap}, false),
    do: spins >= cap

  def expired?(%__MODULE__{synced: false, spins: spins, bootstrap_spins: cap}, true),
    do: spins >= cap

  @doc "A human readable summary for logging."
  @spec describe(t(), boolean()) :: String.t()
  def describe(%__MODULE__{synced: true} = idle, _handshaken?) do
    "idle timeout (spins #{idle.spins}/#{idle.max_spins}, synced: true)"
  end

  def describe(%__MODULE__{} = idle, handshaken?) do
    cap = if handshaken?, do: idle.bootstrap_spins, else: idle.handshake_spins
    "idle timeout (spins #{idle.spins}/#{cap}, synced: false)"
  end
end
