-module(configuration).
-export([
  basic_server_configuration/0,
  blocking_server/0,
  hybrid_server/0,
  non_blocking_server/0
]).


basic_server_configuration() -> [
  binary,
  {packet, 4},
  {reuseaddr, true}
].

%%% Blocking vs. Non-blocking Server
%% The server may be opened in a mode that is active, passive or active once.
%% The difference is how messages are received from the socket:
%%
%% ACTIVE:
%%   the controlling process will receive messages as they arrive
%%   without possibility to control the incoming data flow. A rouge
%%   client could send thousands of messages and flood the controlling
%%   process.
%%   This is called NON-BLOCKING
%%
%% PASSIVE:
%%   The controlling process has to call `gen_tcp:recv(Socket, N)`
%%   to receive data from the socket and will then try to receive
%%   exactly N bytes (N = 0 means all available bytes are returned).
%%   This is called BLOCKING.
%%
%% HYBRID:
%%   A passive (blocking) server can only receive data from one socket.
%%   This is useless for servers with multiple sockets.
%%   The hybrid 'active once' approach will be active until the first message
%%   arrives and than go passive. To receive the next message the controlling
%%   process must explicitly reset the configuration (with `inet:setopts/2`.
%%   The system will block until that happens.
%%   This is called PARTIAL BLOCKING.
%%

blocking_server()     -> [{active, false} | basic_server_configuration()].
non_blocking_server() -> [{active, true}  | basic_server_configuration()].
hybrid_server()       -> [{active, once}  | basic_server_configuration()].