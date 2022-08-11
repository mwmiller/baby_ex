defmodule Baby.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    # Ensure the startup environment makes some sense
    # The configured spool dir must exist we make no assumptions
    # about its suitability
    proper_dir = Application.get_env(:baobab, :spool_dir) |> Path.expand()

    case File.exists?(proper_dir) do
      true -> :ok
      false -> File.mkdir_p(proper_dir)
    end

    # The configured identity must exist
    whoami = Application.get_env(:baby, :identity)

    case Baobab.identity_key(whoami, :public) do
      :error -> Baobab.create_identity(whoami)
      _ -> :ok
    end

    :ranch.start_listener(
      :baby,
      :ranch_tcp,
      [port: Application.get_env(:baby, :port)],
      Baby.Connection,
      whoami
    )

    children = [
      {Baby.Monitor, %{cryouts: Application.get_env(:baby, :cryouts)}}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Baby.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
