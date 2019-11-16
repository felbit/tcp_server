-module(tcp_server_test).

-include_lib("eunit/include/eunit.hrl").

start_seq_server_test() ->
  %% Starting the sequential test server
  spawn(fun () -> tcp_server:start_seq_server() end),

  %% Opening a connection and sending a binary to the server
  {ok, Socket} = gen_tcp:connect("localhost", 3000, [binary, {packet, 4}]),
  ok = gen_tcp:send(Socket, iolist_to_binary("Hello, World!")),

  %% checking the response
  receive
    {tcp, Socket, Bin} ->
      Result = binary_to_list(Bin),
      ?assertEqual("HTTP/1.0 200 OK\nContent-Type: text/html\nContent-Length: 13\n\nHello, World!", Result),
      gen_tcp:close(Socket)
    end.
