-module(nodebalancer).
-export().

%%Author: Tim Jonasson
%%Version: 1.0

% Makes it possible to initialize the nodes with an escript command
main(_) -> 
  init().

%Initializes the node tcp connection.  
init() -> 
  {ok, ListenSocket} = gen_tcp:listen(8041, [{active,true}, binary]),
  loop(ListenSocket).
  
%Accepts connections and spawns a proccess for it
loop(ListenSocket) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  Pid = spawn(fun() -> nodecoordinator:init(Socket) end),
  %Makes the Pid recieve everything sent to the given socket
  gen_tcp:controlling_process(Socket, Pid),
  loop(ListenSocket).