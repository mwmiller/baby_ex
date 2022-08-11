import Config

config :baobab,
  spool_dir: "~/.baobab"

config :baby,
  identity: "mwm",
  port: 8483,
  cryouts: [[host: "quagga.nftease.online", port: 8483, period_ms: 419_563]]
