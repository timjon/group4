-module(usercoordinator).
-export([init/1]).

init(Socket) -> 
  inet:setopts(Socket, [{active, true}]),

  io:format("hello ~p", [inet:peername(Socket)]),
    
  loop(Socket, []).
  
loop(Socket, Diagrams) -> 
  receive
    {tcp, Port, Info} -> io:format("Received this: ~n ~p", [Info]),
	  {ok, Scanned, _} = erl_scan:string(binary_to_list(Info)),
	  case erl_parse:parse_term(Scanned ++ [{dot,0}]) of 
	    {ok, {N, next_message}} -> 
		    case find_diagram(N, Diagrams) of
			  not_created -> rip;
			  Pid         -> 
			    io:format("Matched with pid ~n"),
				Pid ! {next_message, self()},
			  receive
			    ok -> ok
			  end,
			  io:format("Received ok ~n")
			end,
			loop(Socket, Diagrams);
		{ok, {Did, Classes, Messages}} ->
	      
	      X = io_lib:format("~p", [Classes]) ++ "~",
	      gen_tcp:send(Port, X),
	  
	      case find_diagram(Did, Diagrams) of 
	        not_created -> loop(Socket, [{Did, spawn(fun () -> diagramcoordinator:init({Classes, Messages}) end)}| Diagrams]);
	        _           -> loop(Socket, Diagrams)
	      end
	  end;

	{tcp_closed, Port} -> 
	  io:format("Closed ~p", [Port])
  end.

 
find_diagram(_, []) -> not_created;
find_diagram(Diagram_id, [{Diagram_id, Pid} | _]) -> Pid;
find_diagram(Diagram_id, [_| Diagrams])  -> find_diagram(Diagram_id, Diagrams).

send_back(Port, Message) -> 
gen_tcp:send(Port, Message).

send_message(Did, Diagrams, Port) -> 
  case find_diagram(Did, Diagrams) of 
    not_created -> send_back(Port, "invalid_diagram_id ~n");
			
	Pid         -> Pid ! {next_message, Pid, self()},
	  receive 
	  {ok, Pid} -> ok
	  end
  end.