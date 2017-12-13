-module(nodebalancer).
-export([main/1, init/1]).

%%Author: Tim Jonasson
%%Version: 1.0

% Makes it possible to initialize the nodes with an escript command
main(String) -> 
  init(String).

%Initializes the node tcp connection.  
init(String) -> 
  {Port, _} = string:to_integer(hd(String)),
  {ok, ListenSocket} = gen_tcp:listen(Port, [{active,true}, binary]),
  loop(ListenSocket).
  
%Accepts connections and spawns a proccess for it
loop(ListenSocket) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  Pid = spawn(fun() -> nodecoordinator:init(Socket) end),
  %Makes the Pid recieve everything sent to the given socket
  gen_tcp:controlling_process(Socket, Pid),
  loop(ListenSocket).