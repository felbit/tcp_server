-module(tcp_server).

-export([
  loop/1,
  nano_client_eval/1,
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

server_configuration() ->
  [binary, {packet, 4}, {reuseaddr, true}, {active, true}].

start_seq_server() ->
  {ok, Listen} = gen_tcp:listen(3000, server_configuration()),
  seq_loop(Listen).

seq_loop(Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  loop(Socket),
  seq_loop(Listen).

%%% PARALLEL SERVER
%% Accepts multiple connections at the same time.
%% Spawns a new process for every accepted connection.

start_parallel_server() ->
  {ok, Listen} = gen_tcp:listen(3000, server_configuration()),
  spawn(fun() -> par_connect(Listen) end).

par_connect(Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  spawn(fun() -> par_connect(Listen) end),
  loop(Socket).

loop(Socket) ->
  receive
    {tcp, Socket, Bin} ->
      io:format("Server received binary = ~p~n", [Bin]),
      Str = binary_to_term(Bin),
      io:format("Server (unpacked)  ~p~n", [Str]),
      Reply = string2value(Str),
      io:format("Server replying = ~p~n", [Reply]),
      gen_tcp:send(Socket, term_to_binary(Reply)),
      loop(Socket);
    {tcp_closed, Socket} ->
      io:format("Server socket closed.~n")
  end.

%% Helper
string2value(Str) ->
  {ok, Tokens, _}   = erl_scan:string(Str ++ (".")),
  {ok, Exprs}       = erl_parse:parse_exprs(Tokens),
  Bindings          = erl_eval:new_bindings(),
  {value, Value, _} = erl_eval:exprs(Exprs, Bindings),
  Value.