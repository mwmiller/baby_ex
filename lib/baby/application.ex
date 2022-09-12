defmodule Baby.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, args \\ []) do
    {port, identity, clump_id, cryouts} = setup_from_args(args)
    # The configured identity must exist
    :ranch.start_listener(:baby, :ranch_tcp, [port: port], Baby.Connection,
      identity: identity,
      clump_id: clump_id
    )

    children = [
      {Baby.Monitor, %{cryouts: cryouts, identity: identity, clump_id: clump_id}}
    ]

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

  # It's considered bad form to muck with configs outside of our own application
  # But it's hard to configure Baobab itself since it's a stateless library
  # Plus, I wrote it, so I'm not going to complain to myself... yet.
  defp setup_from_args(args) do
    # Ensure the startup environment makes some sense
    # The configured spool dir must exist we make no assumptions
    # about its suitability
    baobab_spool =
      case Keyword.get(args, :spool_dir) do
        nil ->
          Application.get_env(:baobab, :spool_dir)

        path ->
          :ok = Application.put_env(:baobab, :spool_dir, path)
          path
      end

    case File.exists?(baobab_spool) do
      true -> :ok
      false -> File.mkdir_p(baobab_spool)
    end

    whoami =
      case Keyword.get(args, :identity) do
        nil ->
          Application.get_env(:baby, :identity)

        id ->
          Application.put_env(:baby, :identity, id)
          id
      end

    case Baobab.identity_key(whoami, :public) do
      :error -> Baobab.create_identity(whoami)
      _ -> :ok
    end

    clump_id =
      case Keyword.get(args, :clump_id) do
        nil ->
          Application.get_env(:baby, :clump_id)

        cid ->
          Application.put_env(:baby, :clump_id, cid)
          cid
      end

    port = Keyword.get(args, :port, Application.get_env(:baby, :port, 8483))
    cryouts = Keyword.get(args, :cryouts, Application.get_env(:baby, :cryouts, []))
    {port, whoami, clump_id, cryouts}
  end
end
