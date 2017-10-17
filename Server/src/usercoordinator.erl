-module(usercoordinator).
-export([init/1]).

init(Socket) -> 
  inet:setopts(Socket, [{active, true}]),
  loop(Socket).
  
loop(Socket) -> 
  timer:sleep(1000),
  receive
    {tcp, Port, Info} -> io:format("Received this: ~p", [Info]),
	  {ok, Scanned, _} = erl_scan:string(binary_to_list(Info)),
	  {ok, Parsed} = erl_parse:parse_term(Scanned ++ [{dot,0}]),
	  io:format("Parsed this ~p", [Parsed]),
	  spawn(fun () -> diagramcoordinator:init(Parsed) end),
	  loop(Socket);
	{tcp_closed, Port} -> io:format("Closed ~p", [Port])
  end,
  
  loop(Socket).
%diagramcoordinator:init([u1, u2], [{u1, u2, hello}, {u2, u1, hello}])

