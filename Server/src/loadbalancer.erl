-module(loadbalancer).
-export([init/0]).

init() -> 
	Pid = {ok, ListenSocket} = gen_tcp:listen(8040, [{active,true}, binary]),
    loop(ListenSocket).
  
loop(ListenSocket) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  spawn(fun() -> loop(ListenSocket) end),
  usercoordinator:init(Socket).
