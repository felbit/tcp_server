# tcp_server

A simple tcp server written as OTP library in Erlang.

## Build

    $ rebar3 compile

## Test

To test the current server you'll need two shells, so open two terminal windows in the root directory of `tcp_server` and type in each:

```
$ rebar3 shell
```

```
%% In the first window
> tcp_server:start_nano_server().
%% Nothing will happen here (it's waiting) ...
```

```
%% In the second window
> tcp_server:nano_client_eval("list_to_tuple([2+3*4, 10+23])").
Client received binary = <<131,104,2,97,14,97,33>>
Client result = {14,33}
ok
%% Server will close the connection now.
```

