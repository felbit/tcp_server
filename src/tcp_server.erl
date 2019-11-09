-module(tcp_server).

-export([]).

start_nano_server() ->
  {ok, Listen} = gen_tcp:listen(3000, [binary, {packet, 4},
                                       {reuseaddr, true},
                                       {active, true}]),
  {ok, Socket} = gen_tcp:accept(Listen),
  gen_tcp:close(Listen),
  loop(Socket).

loop(Socket) ->
  receive
    {tcp, Socket, Bin} ->
      io:format("Server received binary = ~p~n", [Bin]),
      Str = binary_to_term(Bin),
      io:format("Server (unpacked)  ~p~n", [Str]),
      Reply = lib_misc:string2value(Str),
      io:format("Server replying = ~p~n", [Reply]),
      gen_tcp:send(Socket, term_to_binary(Reply)),
      loop(Socket);
    {tcp_closed, Socket} ->
      io:format("Server socket closed.~n")
  end.
