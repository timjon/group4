-module(loadbalancer).
-export([init/0]).

%%Author: Tim Jonasson
%%Version: 1

%Initializes the server
init() -> 
  {ok, ListenSocket} = gen_tcp:listen(8040, [{active,true}, binary]),
  loop(ListenSocket).
  
%Accepts connections and spawns a proccess for it
loop(ListenSocket) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  spawn(fun() -> loop(ListenSocket) end),
  usercoordinator:init(Socket).