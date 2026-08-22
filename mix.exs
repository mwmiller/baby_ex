defmodule Baby.MixProject do
  use Mix.Project

  def project do
    [
      app: :baby,
      version: "0.39.0",
      elixir: "~> 1.18",
      name: "Baby",
      source_url: "https://github.com/mwmiller/baby_ex",
      start_permanent: Mix.env() == :prod,
      description: description(),
      package: package(),
      aliases: aliases(),
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger, :ranch]
    ]
  end

  def cli do
    [preferred_envs: [precommit: :test]]
  end

  defp aliases do
    [
      precommit: [
        "format --check-formatted",
        "compile --warnings-as-errors --force",
        "credo --strict",
        "test"
      ]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:baobab, "~> 0.35"},
      {:blake2, "~> 1.0"},
      {:primacy, ">= 0.0.0"},
      {:stlv, "~> 1.0"},
      {:varu64, "~> 1.0"},
      {:kcl, "~> 1.5"},
      # Third-party
      {:cbor, "~> 1.0"},
      {:mdns_lite, "~> 0.9"},
      {:ranch, "~> 1.8"},
      {:replayq, "~> 0.3.7"},
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false},
      {:ex_doc, ">= 0.0.0", only: :dev, runtime: false}
    ]
  end

  defp description do
    """
    Bushbaby Automated Bamboo Yields
    """
  end

  defp package do
    [
      files: ["lib", "mix.exs", "README*", "LICENSE*"],
      maintainers: ["Matt Miller"],
      licenses: ["MIT"],
      links: %{
        "GitHub" => "https://github.com/mwmiller/baby_ex"
      }
    ]
  end
end
