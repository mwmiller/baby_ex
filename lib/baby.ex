defmodule Baby do
  require Logger

  @moduledoc """
  Bushbaby Automated Bamboo Yields
  """

  @doc """
  Connect to a remote hort and port

  Keyword id_options:
  - clump_id
  - identity
  """
  def connect(host, port, id_options \\ [])

  def connect(host, port, id_options) when is_binary(port),
    do: connect(host, String.to_integer(port), id_options)

  def connect(host, port, id_options) when is_binary(host),
    do: connect(host_to_ip(host), port, id_options)

  def connect(host, port, id_options) do
    # This mucks about with state it has no business touching
    # If the Application is running, it's a problem.
    opts = Keyword.delete(id_options, :spool_dir)
    global_setup(opts)

    Baby.Connection.start_link(
      host: host,
      port: port,
      identity: Keyword.get(opts, :identity),
      clump_id: Keyword.get(opts, :clump_id)
    )
  end

  @doc false
  def global_setup(args) do
    baobab_spool =
      case Keyword.get(args, :spool_dir) do
        nil -> Application.get_env(:baby, :spool_dir)
        path -> path
      end
      |> Path.expand()

    :ok = Application.put_env(:baobab, :spool_dir, baobab_spool)

    # Allow for connections to clumps which are not globally configured
    # This might get messy
    clumps =
      case Keyword.get(args, :clump_id) do
        nil -> Application.get_env(:baby, :clumps, [])
        kw -> [[id: kw]]
      end

    for clump <- clumps do
      case Keyword.get(clump, :id) do
        nil -> :ok
        clump_id -> File.mkdir_p(Path.join([baobab_spool, clump_id]))
      end
    end
  end

  @doc false
  def host_to_ip(host) do
    where = to_charlist(host)

    case :inet.gethostbyname(where) do
      {:ok, {:hostent, _, _, _, _, [addr | _more]}} ->
        addr

      _ ->
        case :inet.parse_ipv6_address(where) do
          {:ok, addr} -> addr
          _ -> :error
        end
    end
  end
end
