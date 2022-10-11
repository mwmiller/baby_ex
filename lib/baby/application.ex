defmodule Baby.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, args \\ []) do
    clumps = Baby.global_setup(args)
    # Handle singular provided clump definition

    children =
      clumps_setup(clumps)
      |> Enum.reduce([], fn {port, identity, clump_id, cryouts}, a ->
        # The configured identity must exist
        :ranch.start_listener(:baby, :ranch_tcp, [port: port], Baby.Connection,
          identity: identity,
          clump_id: clump_id
        )

        [
          Supervisor.child_spec(
            {Baby.Monitor, %{cryouts: cryouts, identity: identity, clump_id: clump_id}},
            id: String.to_atom(clump_id)
          )
          | a
        ]
      end)

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Baby.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start, [nil, opts]},
      type: :worker,
      restart: :permanent,
      shutdown: 500
    }
  end

  defp clumps_setup(clumps, acc \\ [])
  defp clumps_setup([], acc), do: acc

  defp clumps_setup([clump | rest], acc) do
    whoami = Keyword.get(clump, :controlling_identity, Application.get_env(:baby, :identity))

    case Baobab.identity_key(whoami, :public) do
      :error ->
        case Keyword.get(clump, :controlling_secret) do
          nil -> Baobab.create_identity(whoami)
          sk -> Baobab.create_identity(whoami, sk)
        end

      _ ->
        :ok
    end

    clump_id = Keyword.get(clump, :id, Application.get_env(:baby, :clump_id))
    port = Keyword.get(clump, :port, Application.get_env(:baby, :port, 8483))
    cryouts = Keyword.get(clump, :cryouts, Application.get_env(:baby, :cryouts, []))
    clumps_setup(rest, [{port, whoami, clump_id, cryouts} | acc])
  end
end
