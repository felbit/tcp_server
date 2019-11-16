-module(tcp_server).

-export([
  loop/1,
  start_parallel_server/0,
  start_seq_server/0
]).

%%% SEQUENTIAL SERVER
%% Accepts one connection at a time.

%% Stopping the server:
%% kill the process that started the server, since gen_tcp links
%% itself to the controlling process. If the process dies, it
%% closes the socket.

%% Note: the controlling process for a socket can be changed
%% to NewPid by calling `gen_tcp:controlling_process(Socket, NewPid)`.

start_seq_server() ->
  {ok, Listen} = gen_tcp:listen(3000, configuration:hybrid_server()),
  seq_loop(Listen).

seq_loop(Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  loop(Socket),
  seq_loop(Listen).

%%% PARALLEL SERVER
%% Accepts multiple connections at the same time.
%% Spawns a new process for every accepted connection.
%% It might be a good idea to limit the number of processes, though.

start_parallel_server() ->
  {ok, Listen} = gen_tcp:listen(3000, configuration:hybrid_server()),
  spawn(fun() -> par_connect(Listen) end).

par_connect(Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  spawn(fun() -> par_connect(Listen) end),
  loop(Socket).

loop(Socket) ->
  receive
    {tcp, Socket, Bin} ->

      %% do something with the data (this is currently just dummy code ...)
      io:format("Server received binary = ~p~n", [Bin]),
      Content = binary_to_list(Bin),
      gen_tcp:send(Socket, response(Content)),

      %% Partial blocking server needs reactivation of active message reception
      inet:setopts(Socket, [{active, once}]),
      loop(Socket);
    {tcp_closed, Socket} ->
      io:format("Server socket closed.~n")
  end.

response(Payload) ->
  P = iolist_to_binary(Payload),
  iolist_to_binary(
    io_lib:fwrite(
      "HTTP/1.0 200 OK\nContent-Type: text/html\nContent-Length: ~p\n\n~s",
      [size(P), P])).
