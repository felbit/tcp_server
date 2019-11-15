-module(tcp_server).

-export([loop/1, nano_client_eval/1, start_seq_server/0]).

%%% SEQUENTIAL SERVER
%% Accepts one connetcion at a time.

start_seq_server() ->
  {ok, Listen} = gen_tcp:listen(3000, [binary, {packet, 4},
                                       {reuseaddr, true},
                                       {active, true}]),
  seq_loop(Listen).

seq_loop(Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  loop(Socket),
  seq_loop(Listen).




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


%% Client for testing
nano_client_eval(Str) ->
  {ok, Socket} =
    gen_tcp:connect("localhost", 3000,
                    [binary, {packet, 4}]),
  ok = gen_tcp:send(Socket, term_to_binary(Str)),
  receive
    {tcp, Socket, Bin} ->
      io:format("Client received binary = ~p~n", [Bin]),
      Val = binary_to_term(Bin),
      io:format("Client result = ~p~n", [Val]),
      gen_tcp:close(Socket)
    end.

%% Helper
string2value(Str) ->
  {ok, Tokens, _}   = erl_scan:string(Str ++ (".")),
  {ok, Exprs}       = erl_parse:parse_exprs(Tokens),
  Bindings          = erl_eval:new_bindings(),
  {value, Value, _} = erl_eval:exprs(Exprs, Bindings),
  Value.