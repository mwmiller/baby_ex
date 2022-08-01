defmodule Baby.MixProject do
  use Mix.Project

  def project do
    [
      app: :baby,
      version: "0.1.0",
      elixir: "~> 1.14-dev",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {Baby.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:baobab, "~> 0.2.0"},
      {:cbor, "~> 1.0"},
      {:stlv, "~> 1.0"},
      {:varu64, "~> 1.0"},
      {:yamfhash, "~> 1.0"}
    ]
  end
end
