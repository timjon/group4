-module(loadbalancer).
-export([init/0]).

init() -> 
	Pid = {ok, ListenSocket} = gen_tcp:listen(8040, [{active,true}, binary]),
    loop(ListenSocket).
  
loop(ListenSocket) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  spawn(fun() -> loop(ListenSocket) end),
  usercoordinator:init(Socket).

  
handle(Socket) ->
  inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket, Msg} ->
      gen_tcp:send(Socket, "fuck you, you are not going to get this shit anyway ~"),
      handle(Socket)
  end.

%loop(ListenSocket) -> 
 % {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
 % P = spawn(fun () -> usercoordinator:init(AcceptSocket) end),
 % P = usercoordinator:init(AcceptSocket),
 % io:format("Proccess:  \n", [P]),
 % gen_tcp:controlling_process(AcceptSocket, P),
 % loop(ListenSocket).