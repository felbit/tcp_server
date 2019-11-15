-module(helpers).
-export([string_to_value/1]).

string_to_value(Str) ->
  {ok, Tokens, _}   = erl_scan:string(Str ++ (".")),
  {ok, Exprs}       = erl_parse:parse_exprs(Tokens),
  Bindings          = erl_eval:new_bindings(),
  {value, Value, _} = erl_eval:exprs(Exprs, Bindings),
  Value.