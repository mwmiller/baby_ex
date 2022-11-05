defmodule Baby.MixProject do
  use Mix.Project

  def project do
    [
      app: :baby,
      version: "0.5.0",
      elixir: "~> 1.13",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger, :ranch]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:baobab, "~> 0.15.0"},
      {:stlv, "~> 1.0"},
      {:varu64, "~> 1.0"},
      {:yamfhash, "~> 1.0"},
      # Third-party
      {:cbor, "~> 1.0"},
      {:enacl, "~> 1.2"},
      {:ranch, "~> 1.8"}
    ]
  end
end
