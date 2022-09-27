import Config

config :baby,
  spool_dir: "~/.baobab",
  clumps: [
    [
      id: "Quagga",
      controlling_identity: "mwm",
      port: 8483,
      cryouts: [
        [host: "quagga.nftease.online", port: 8483, period: {7, :minute}],
        [host: "zebra.nftease.online", port: 8483, period: {17, :minute}]
      ]
    ]
  ]
