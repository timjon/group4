-module(loadbalancer).
-export([init/0]).

%%Author: Tim Jonasson
%%Collaborator: Sebastian Fransson
%%Version: 1.1

%Initializes the server
init() -> 
  {ok, ListenSocket} = gen_tcp:listen(8040, [{active,true}, binary]),
  Pid = spawn(fun() -> lobbycoordinator:init() end),
  monitor(process,Pid),
  register(lobbycoordinator, Pid),
  loop(ListenSocket).
  
%Accepts connections and spawns a proccess for it
loop(ListenSocket) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  Pid = spawn(fun() -> usercoordinator:init(Socket) end),
  gen_tcp:controlling_process(Socket, Pid),
  loop(ListenSocket).