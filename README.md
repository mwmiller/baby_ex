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

## Wire buffer

Inbound bytes that have not yet formed a complete protocol frame are buffered
in the connection's `wire` state. A peer that dribbles undecodable bytes could
otherwise grow this buffer without bound, so it is capped:

  * `:wire_cap` — maximum buffered bytes per connection before the connection
    is dropped. Read from Application config (`config :baby, wire_cap: ...`)
    at connection startup, overridable per-connection. Default: 32MB.

The cap sits comfortably above the largest legitimate frame (a single message
may carry an entire log), so it should only trip for garbage; note the
[protocol spec](Bushbaby.md) guidance on authoring log entries that fit within
peers' limits.

## Listener

The ranch listener that accepts peer connections has a deliberate ceiling on
concurrent connections, so a flood of anonymous connections cannot exhaust
file descriptors or slot-bound the legitimate peer:

  * `:max_connections` — maximum concurrent connections accepted per listener.
    Read from Application config (`config :baby, max_connections: ...`) at
    startup, overridable per-clump. Default: 256 (ranch's own default is 1024).

```elixir
config :baby, max_connections: 256
```

## Local network discovery (mDNS)

Clumps can advertise themselves to peers on the local network and can find
those peers without statically configured addresses. This is built on
[`mdns_lite`](https://hex.pm/packages/mdns_lite), which is started
automatically when any clump needs it. Services are advertised as standard
`_bushbaby._tcp` DNS-SD records whose TXT payload carries the announcing
clump's `clump_id`, so they are visible to any mDNS tooling
(`dns-sd`, Avahi, etc.), not just other `Baby` nodes.

Per-clump configuration (`config :baby, clumps: [...]`):

  * `announce` — `true` advertises this clump's listener via mDNS; a keyword
    list of `Baby.Mdns.announce/3` options (e.g. `instance:`) customizes the
    advertisement. Default: `false`.
  * Within a `cryouts` entry, `mdns:` — `true` (or keyword options) turns
    that cryout into a "meta cryout": instead of dialing a fixed
    `{host, port}`, the monitor periodically browses the local network and
    connects to every discovered peer of the same clump it isn't already
    talking to. A `period` may be given either at the cryout's top level or
    inside the `mdns` options; unconfigured meta cryouts cycle about once a
    minute (a random prime number of seconds near 60, so many peers do not
    browse in lockstep), while fixed-host cryouts default to `{17, :minute}`.

```elixir
config :baby,
  clumps: [
    [
      id: "Quagga",
      controlling_identity: my_identity,
      port: 8483,
      announce: true,
      cryouts: [[mdns: [period: {5, :minute}]]]
    ]
  ]
```

Because discovery re-runs on every meta-cryout cycle, clump-mates that come
and go are picked up (and re-dialed) automatically, exactly as fixed-host
cryouts re-establish dropped connections.
