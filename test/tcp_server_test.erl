-module(tcp_server_test).

-include_lib("eunit/include/eunit.hrl").

start_seq_server_test() ->
  %% Starting the sequential test server
  spawn(fun () -> tcp_server:start_seq_server() end),

  %% Opening a connection and sending a binary to the server
  {ok, Socket} = gen_tcp:connect("localhost", 3000, [binary, {packet, 4}]),
  ok = gen_tcp:send(Socket, term_to_binary("list_to_tuple([1, 2])")),

  %% checking the response
  receive
    {tcp, Socket, Bin} ->
      Result = binary_to_term(Bin),
      ?assertEqual({1, 2}, Result),
      gen_tcp:close(Socket)
    end.
