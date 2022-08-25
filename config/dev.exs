import Config

config :baobab,
  spool_dir: "~/.baobab"

config :baby,
  identity: "mwm",
  clump_id: "Quagga",
  port: 8483,
  cryouts: [
    [host: "quagga.nftease.online", port: 8483, period: {7, :minute}],
    [host: "zebra.nftease.online", port: 8483, period: {17, :minute}]
  ]
