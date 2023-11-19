import Config

config :baby,
  spool_dir: "~/.baobab",
  clumps: [
    [
      id: "Quagga",
      controlling_identity: "mwm",
      port: 8483,
      cryouts: [
        [host: "quagga.zebrine.net", port: 8483, period: {7, :minute}],
        [host: "zebra.zebrine.net", port: 8483, period: {17, :minute}]
      ]
    ],
    [
      id: "Pitcairn",
      controlling_identity: "mwm",
      port: 8485,
      cryouts: [
        [host: "sally.zebrine.net", port: 8485, period: {7, :minute}]
      ]
    ]
  ]
