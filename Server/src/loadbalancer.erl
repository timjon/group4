-module(loadbalancer).
-export([init/0]).
-import(usercoordinator, [init/1]).

init() -> 
	Pid = spawn_link(fun() ->
        {ok, ListenSocket} = gen_tcp:listen(8040, [{active,true}, binary]),
        spawn(fun() -> {ok, AcceptSocket} = acceptor(ListenSocket) end),
        timer:sleep(infinity)
    end),
    {ok, Pid}.
  
acceptor(ListenSocket) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  spawn(fun() -> acceptor(ListenSocket) end),
  handle(Socket).

  
handle(Socket) ->
  inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket, <<"quit", _/binary>>} ->
      gen_tcp:close(Socket);
    {tcp, Socket, Msg} ->
	  io:format("hello"),
      gen_tcp:send(Socket, "fuck you, you are not going to get this shit anyway ~"),
      handle(Socket)
  end.

loop(ListenSocket) -> 
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  P = spawn(fun () -> usercoordinator:init(AcceptSocket) end),
  P = usercoordinator:init(AcceptSocket),
  io:format("Proccess:  \n", [P]),
  gen_tcp:controlling_process(AcceptSocket, P),
  loop(ListenSocket).