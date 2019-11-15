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
  % Setting options can also be done by inet:setopts/2:
  %inet:setopts(Socket, configuration:hybrid_server()),
  spawn(fun() -> par_connect(Listen) end),
  loop(Socket).

loop(Socket) ->
  receive
    {tcp, Socket, Bin} ->

      %% do something with the data (this is currently just dummy code ...)
      io:format("Server received binary = ~p~n", [Bin]),
      Str = binary_to_term(Bin),
      io:format("Server (unpacked)  ~p~n", [Str]),
      Reply = helpers:string_to_value(Str),
      io:format("Server replying = ~p~n", [Reply]),
      gen_tcp:send(Socket, term_to_binary(Reply)),
      %% end of dummy code.

      %% Partial blocking server needs reactivation of active message reception
      inet:setopts(Socket, [{active, once}]),
      loop(Socket);
    {tcp_closed, Socket} ->
      io:format("Server socket closed.~n")
  end.
