-module(usercoordinator).
-export([init/1]).

%%Author: Tim Jonasson
%%Version: 1.1

%Initializes the usercoordinator
init(Socket) -> 
  inet:setopts(Socket, [{active, true}]),
  loop(Socket, []).

  
loop(Socket, Diagrams) -> 
  receive
    {tcp, Port, Info} -> 
	  %Scans the info read from the tcp connection
	  {ok, Scanned, _} = erl_scan:string(binary_to_list(Info)),
	  
	  %Turns the scanned info into erlang format
	  case erl_parse:parse_term(Scanned ++ [{dot,0}]) of 
	    
		%If the message is the Diagram id and the atom next_message
		{ok, {N, next_message}} -> 
		    %Checks if the diagram exists
			case find_diagram(N, Diagrams) of
			  not_created -> not_ok;
			  Pid         -> 
				%Makes the server simulate the next message
				Pid ! {next_message, self()},
			    %Confirmation that the diagram coordinator received the message
				receive
			      ok -> ok
			    end,
				%Receives the result from the diagram coordinator and sends it to the client
			    receive
		          {message_sent, From, To, Message, Message_number} -> 
				    %Turns the result into binary
					%The character ~ is used as the stop character for when the client should stop reading from the tcp connection
					Format_result = io_lib:format("~p", [{From, To, Message, Message_number}]) ++ "~",
		            %Sends it to the client
					gen_tcp:send(Port, [Format_result]);
				  {simulation_done, Message_number} -> 
					%Turns the result into binary
					Format_result = io_lib:format("~p", ["simulation_finished, " ++ integer_to_list(Message_number)]),
	                %Sends it to the client
					gen_tcp:send(Port, [Format_result ++ "~"])
	            end
			end;
		%This will be selected if its a new diagram selected
		{ok, {Did, Class_names, Classes, Messages}} ->
	      %Sends the class names and messages to the client
		  %The character ~ is used as the stop character for when the client should stop reading from the tcp connection
		  Format_result = io_lib:format("~p", [{Class_names, Messages}]) ++ "~",
	      gen_tcp:send(Port, Format_result),
	      %Spawns a diagram coordinator for this diagram if it doesnt exist already 
		  case find_diagram(Did, Diagrams) of 
	        not_created -> loop(Socket, [{Did, spawn(fun () -> diagramcoordinator:init({Classes, Messages}) end)}| Diagrams])
	      end
	  end;

	%This case happens if the connection to the client is lost
	{tcp_closed, _Port} -> 
	  shut_down
  end,
  loop(Socket, Diagrams).
 
%Finds the correct diagram from the given list
find_diagram(_, []) -> not_created;
find_diagram(Diagram_id, [{Diagram_id, Pid} | _]) -> Pid;
find_diagram(Diagram_id, [_| Diagrams])  -> find_diagram(Diagram_id, Diagrams).
