defmodule Baby.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(type, args \\ [])
  def start(type, []), do: start(type, Application.get_all_env(:baby))

  def start(_type, args) do
    spool_path = Keyword.get(args, :spool_dir)
    baobab_spool = Path.expand(spool_path)
    :ok = Application.put_env(:baobab, :spool_dir, baobab_spool)
    # It is presumed that all available clumps are established here
    # A later `Baby.connect` will not create missing clumps
    clumps = Keyword.get(args, :clumps, [])

    for clump <- clumps do
      case Keyword.get(clump, :id) do
        nil -> :ok
        clump_id -> File.mkdir_p(Path.join([baobab_spool, clump_id]))
      end
    end

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

    opts = [strategy: :one_for_one, name: Baby.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start, [:normal, opts]},
      type: :worker,
      restart: :permanent,
      shutdown: 500
    }
  end

  defp clumps_setup(clumps, acc \\ [])
  defp clumps_setup([], acc), do: acc

  defp clumps_setup([clump | rest], acc) do
    whoami = Keyword.get(clump, :controlling_identity, "default")

    case Baobab.identity_key(whoami, :public) do
      :error ->
        case Keyword.get(clump, :controlling_secret) do
          nil -> Baobab.create_identity(whoami)
          sk -> Baobab.create_identity(whoami, sk)
        end

      _ ->
        :ok
    end

    clump_id = Keyword.get(clump, :id)
    port = Keyword.get(clump, :port, 8483)
    cryouts = Keyword.get(clump, :cryouts, [])
    clumps_setup(rest, [{port, whoami, clump_id, cryouts} | acc])
  end
end
