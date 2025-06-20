defmodule Baby.MixProject do
  use Mix.Project

  def project do
    [
      app: :baby,
      version: "0.34.0",
      elixir: "~> 1.18",
      name: "Baby",
      source_url: "https://github.com/mwmiller/baby_ex",
      start_permanent: Mix.env() == :prod,
      description: description(),
      package: package(),
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
      {:baobab, "~> 0.34"},
      {:blake2, "~> 1.0"},
      {:primacy, ">= 0.0.0"},
      {:stlv, "~> 1.0"},
      {:varu64, "~> 1.0"},
      {:kcl, "~> 1.0"},
      # Third-party
      {:cbor, "~> 1.0"},
      {:ranch, "~> 1.8"},
      {:replayq, "~> 0.3.7"},
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
