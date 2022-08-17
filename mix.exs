defmodule Baby.MixProject do
  use Mix.Project

  def project do
    [
      app: :baby,
      version: "0.1.0",
      elixir: "~> 1.13",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger, :ranch],
      mod: {Baby.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:baobab, "~> 0.9.0"},
      {:kcl, "~> 1.0"},
      {:stlv, "~> 1.0"},
      {:varu64, "~> 1.0"},
      {:yamfhash, "~> 1.0"},
      # Third-party
      {:cbor, "~> 1.0"},
      {:ranch, "~> 2.0"}
    ]
  end
end
