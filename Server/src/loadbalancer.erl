-module(loadbalancer).
-export([init/0]).
-import(usercoordinator, [init/1]).

init() -> 
  {ok, ListenSocket} = gen_tcp:listen(8040, [{active,true}, binary]),
  loop(ListenSocket).

loop(ListenSocket) -> 
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  P = spawn(fun () -> usercoordinator:init(AcceptSocket) end),
  io:format("Proccess: ~p", [P]),
  gen_tcp:controlling_process(AcceptSocket, P),
  loop(ListenSocket).