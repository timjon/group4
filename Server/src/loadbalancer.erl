-module(loadbalancer).
-export([init/0]).

%%Author: Tim Jonasson
%%Version: 1.0

%Initializes the server
init() -> 
  {ok, ListenSocket} = gen_tcp:listen(8040, [{active,true}, binary]),
  loop(ListenSocket).
  
%Accepts connections and spawns a proccess for it
loop(ListenSocket) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  Pid = spawn(fun() -> usercoordinator:init(Socket) end),
  gen_tcp:controlling_process(Socket, Pid),
  loop(ListenSocket).