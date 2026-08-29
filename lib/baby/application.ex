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

  If any configured clump's port cannot be bound, the whole start fails with
  `{:error, reason}` and any listeners that already came up are rolled back.
  """

  use Application

  # Ranch acceptors per listener: plenty to keep handshakes moving without
  # burning a process per socket
  @ranch_acceptors 100
  @ranch_transport :ranch_tcp

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

    setups =
      clumps
      |> clumps_setup()
      |> tap(&maybe_start_mdns_lite/1)

    case start_clumps(setups) do
      {:ok, started} ->
        per_clump = Enum.map(started, &monitor_spec/1)
        opts = [strategy: :one_for_one, name: Baby.Supervisor]

        children = [Baby.Connection.Registry, Baby.Log.Acceptor, Baby.Log.Writer] ++ per_clump

        case Supervisor.start_link(children, opts) do
          {:ok, pid} ->
            {:ok, pid}

          {:error, reason} ->
            # The supervisor tree failed to come up; undo the listeners we
            # bound so a retry does not trip over "address already in use"
            stop_clumps(started)
            {:error, reason}
        end

      {:error, reason, started} ->
        # A clump could not bind its port: stop whatever listeners did come
        # up and fail the whole start rather than leave a half-configured app
        stop_clumps(started)
        {:error, reason}
    end
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

  # Bind every clump's listener, stopping at the first failure.  Returns the
  # clumps whose listeners did come up, so a failed start can roll them back.
  defp start_clumps(setups) do
    Enum.reduce_while(setups, {:ok, []}, fn clump, {:ok, started} ->
      %{port: port, clump_id: clump_id, announce: announce} = clump

      case start_listener(clump) do
        {:ok, _ref} ->
          maybe_announce(announce, clump_id, port)
          {:cont, {:ok, [clump | started]}}

        {:error, reason} ->
          {:halt, {:error, reason, started}}
      end
    end)
  end

  # Bind and serve a listener for one configured clump.  The /6 form splits
  # ranch's listener concerns into three groups so nothing is silently folded
  # into the wrong bucket:
  #   - acceptors + transport (positional)
  #   - transport options (socket + ranch listener opts as a map)
  #   - protocol options (handed to Baby.Connection as its handler args)
  defp start_listener(clump) do
    ref = String.to_atom("baby_" <> clump.clump_id)

    transport_opts = %{
      socket_opts: [port: clump.port],
      max_connections: clump.max_connections
    }

    protocol_opts = [identity: clump.identity, clump_id: clump.clump_id]

    case :ranch.start_listener(
           ref,
           @ranch_acceptors,
           @ranch_transport,
           transport_opts,
           Baby.Connection,
           protocol_opts
         ) do
      {:ok, _pid} -> {:ok, ref}
      {:error, reason} -> {:error, reason}
    end
  end

  # The monitor child spec for one bound clump
  defp monitor_spec(clump) do
    Supervisor.child_spec(
      {Baby.Monitor,
       %{
         cryouts: clump.cryouts,
         identity: clump.identity,
         clump_id: clump.clump_id,
         port: clump.port
       }},
      id: String.to_atom(clump.clump_id)
    )
  end

  # Roll back the listeners (and any announcements) for clumps that already
  # bound, used when a later clump or the supervisor fails to come up
  defp stop_clumps(started) do
    Enum.each(started, fn clump ->
      :ranch.stop_listener(String.to_atom("baby_" <> clump.clump_id))

      if clump.announce != false do
        Baby.Mdns.deannounce(clump.clump_id)
      end
    end)
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
