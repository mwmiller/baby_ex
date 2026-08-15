defmodule Baby.ConnectionTest do
  use ExUnit.Case

  alias Baby.Connection

  @default_wire_cap 32 * 1024 * 1024

  setup_all do
    start_supervised!({Baby.Connection.Registry, {Baby.Connection.Registry, :start_link, [[]]}})
    Baobab.Identity.create("connection_test")
    :ok
  end

  describe "wire_cap resolution" do
    test "defaults to 32MB when neither config nor opts provide one" do
      assert wire_cap([]) == @default_wire_cap
    end

    test "reads from per-connection opts" do
      assert wire_cap(wire_cap: 2048) == 2048
    end

    test "reads from Application config" do
      Application.put_env(:baby, :wire_cap, 4096)
      on_exit(fn -> Application.delete_env(:baby, :wire_cap) end)

      assert wire_cap([]) == 4096
    end

    test "per-connection opts take precedence over Application config" do
      Application.put_env(:baby, :wire_cap, 4096)
      on_exit(fn -> Application.delete_env(:baby, :wire_cap) end)

      assert wire_cap(wire_cap: 2048) == 2048
    end
  end

  describe "wire_cap enforcement" do
    test "drops a connection that sends more undecodable bytes than the cap" do
      {:ok, listen} = :gen_tcp.listen(0, [:binary, active: false])
      {:ok, port} = :inet.port(listen)

      pid =
        start_supervised!(
          {Connection,
           host: {127, 0, 0, 1}, port: port, identity: "connection_test", wire_cap: 1024}
        )

      {:ok, sock} = :gen_tcp.accept(listen, 1_000)

      # A frame declaring a huge length, followed by garbage that can never
      # complete it: the buffer grows past the cap and the connection drops.
      garbage = Varu64.encode(5) <> Varu64.encode(1_000_000) <> :binary.copy(<<0>>, 2048)
      :ok = :gen_tcp.send(sock, garbage)

      ref = Process.monitor(pid)
      assert_receive {:DOWN, ^ref, :process, _, :normal}, 1_000
    end
  end

  defp wire_cap(opts) do
    {:ok, listen} = :gen_tcp.listen(0, [:binary, active: false])
    {:ok, port} = :inet.port(listen)

    pid =
      start_supervised!(
        {Connection, opts ++ [host: {127, 0, 0, 1}, port: port, identity: "connection_test"]}
      )

    {_, conn_info} = :sys.get_state(pid)
    :gen_tcp.close(listen)
    conn_info.wire_cap
  end
end
