# Baby

Bushbaby Automated Bamboo Yields

Sync for Bamboo stores using the [Bushbaby protocol](Bushbaby.md)

## Connection tuning

The per-connection idle timeout is governed by four settings, all read from
Application config (`config :baby, ...`) at connection startup, and overridable
per-connection:

  * `:outrate` — milliseconds between outbox ticks. Default: a random prime
    near 75 (jittered so connections don't tick in lockstep).
  * `:handshake_spins` — idle budget, in outbox intervals, before a valid
    `HELLO` has been received. A connection that never proves itself is
    dropped quickly so an anonymous flood cannot pin the listener's connection
    slots. Default: a random prime near 375 (~30s at the default outrate).
  * `:max_spins` — idle budget, in outbox intervals, once the initial
    replication sync has completed. Default: a random prime near 1200.
  * `:bootstrap_spins` — idle budget while the initial sync is still in
    progress, giving the peer time to compute its WANT list before being
    dropped. Default: a random prime near 3000.

The idle counter resets on any send or receive, so the budgets measure
consecutive silence rather than wall-clock time on a busy link. A connection
that exceeds its budget is dropped and re-established on the next cryout.

To estimate wall-clock time, multiply the budget by `:outrate` — e.g. a
default `max_spins` of ~1193 at a default `outrate` of ~80ms is roughly 95
seconds, and a default `bootstrap_spins` of ~2969 is roughly 4 minutes.

```elixir
config :baby,
  outrate: 100,
  handshake_spins: 375,
  max_spins: 1200,
  bootstrap_spins: 5000
```
