-module(usercoordinator).
-export([init/1]).

init(Socket) -> 
  inet:setopts(Socket, [{active, true}]),
  case inet:peername(Socket) of 
    {ok, {Ip, Port}} -> 
	        io:format("ip ~p, port ~p~n", [Ip, Port]),
			gen_tcp:connect(Ip, Port, {active, true});
    {error, Error} ->
            io:format("error ~p~n", [Error])
  end,
  loop(Socket, []).
  
loop(Socket, Diagrams) -> 
  io:format("Parsed this"),
  receive
    {tcp, Port, Info} -> io:format("Received this: ~p", [Info]),
	  {ok, Scanned, _} = erl_scan:string(binary_to_list(Info)),
	  case erl_parse:parse_term(Scanned ++ [{dot,0}]) of
	    
		{ok, {Did, Classes, Messages}} ->
	      io:format("Parsed this ~p", [{Classes, Messages}]),
	      case find_diagram(Did, Diagrams) of 
	        not_created -> loop(Socket, [{Did, spawn(fun () -> diagramcoordinator:init({Classes, Messages}) end)}| Diagrams]);
	        _           -> loop(Socket, Diagrams)
	      end;
		
		{ok, {Did, send_message}} -> 
		  send_message(Did, Diagrams, Port),
		  loop(Socket, Diagrams)

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