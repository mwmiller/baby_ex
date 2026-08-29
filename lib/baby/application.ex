defmodule Baby.Application do
  @moduledoc """
  By design the `Baby` server does not start automatically.

  To use the `:baby` configuration, it may be started via:

  ```
  Baby.Application.start(:normal)
  ```

  One may also craft a custom configuration to be supplied at runtime:

  ```
  config = [spool_dir: "~/.special_bamboo"]
  Baby.Application.start(:normal, config)
  ```
  """

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
        clump_id -> Baobab.create_clump(clump_id)
      end
    end

    per_clump =
      clumps
      |> clumps_setup()
      |> tap(&maybe_start_mdns_lite/1)
      |> Enum.reduce([], fn clump, a ->
        %{
          port: port,
          identity: identity,
          clump_id: clump_id,
          cryouts: cryouts,
          announce: announce
        } = clump

        # The configured identity must exist
        :ranch.start_listener(
          String.to_atom("baby_" <> clump_id),
          :ranch_tcp,
          [port: port],
          [max_connections: clump.max_connections],
          Baby.Connection,
          identity: identity,
          clump_id: clump_id
        )

        maybe_announce(announce, clump_id, port)

        [
          Supervisor.child_spec(
            {Baby.Monitor,
             %{cryouts: cryouts, identity: identity, clump_id: clump_id, port: port}},
            id: String.to_atom(clump_id)
          )
          | a
        ]
      end)

    opts = [strategy: :one_for_one, name: Baby.Supervisor]

    Supervisor.start_link(
      [Baby.Connection.Registry, Baby.Log.Acceptor, Baby.Log.Writer] ++ per_clump,
      opts
    )
  end

  @doc """
  A usable child specification for starting under a supervision tree
  """
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

    case Baobab.Identity.key(whoami, :public) do
      :error -> Baobab.Identity.create(whoami, Keyword.get(clump, :controlling_secret))
      _ -> :ok
    end

    clump_id = Keyword.get(clump, :id)
    max_connections = max_connections(clump)

    setup = %{
      port: Keyword.get(clump, :port, 8483),
      identity: whoami,
      clump_id: clump_id,
      cryouts: Keyword.get(clump, :cryouts, []),
      announce: Keyword.get(clump, :announce, false),
      max_connections: max_connections
    }

    clumps_setup(rest, [setup | acc])
  end

  # The mDNS stack is only started when a clump wants to be visible or
  # to find peers via a meta cryout
  defp maybe_start_mdns_lite(clumps) do
    mdns? = Enum.any?(clumps, fn c -> c.announce != false or mdns_cryout?(c.cryouts) end)

    if mdns?, do: {:ok, _} = Application.ensure_all_started(:mdns_lite)

    :ok
  end

  defp mdns_cryout?(cryouts), do: Enum.any?(cryouts, &Keyword.has_key?(&1, :mdns))

  # `announce` may be `true` or a keyword list of `Baby.Mdns.announce/3`
  # options
  defp maybe_announce(false, _, _), do: :ok

  defp maybe_announce(announce, clump_id, port) when is_list(announce),
    do: Baby.Mdns.announce(clump_id, port, announce)

  defp maybe_announce(_, clump_id, port), do: Baby.Mdns.announce(clump_id, port)

  defp max_connections(clump) do
    case Keyword.get(clump, :max_connections) || Application.get_env(:baby, :max_connections) do
      n when is_integer(n) and n > 0 -> n
      _ -> 256
    end
  end
end
