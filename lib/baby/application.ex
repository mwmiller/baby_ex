defmodule Baby.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    :ranch.start_listener(
      :baby,
      :ranch_tcp,
      [port: Application.get_env(:baby, :port)],
      Baby.Connection,
      Application.get_env(:baby, :identity)
    )

    children = [
      # Starts a worker by calling: Baby.Worker.start_link(arg)
      # {Baby.Worker, arg}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Baby.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
