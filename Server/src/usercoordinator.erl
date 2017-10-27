-module(usercoordinator).
-export([init/1]).

%%Author: Tim Jonasson
%%Version: 2.1

%Initializes the usercoordinator
init(Socket) -> 
  inet:setopts(Socket, [{active, true}]),
  loop(Socket, []).

  
loop(Socket, Diagrams) -> 
  receive
    {tcp, Socket, Info} -> 
	  %Scans the info read from the tcp connection
	  {ok, Scanned, _} = erl_scan:string(binary_to_list(Info)),
	  use_input(erl_parse:parse_term(Scanned ++ [{dot,0}]), Socket, Diagrams);
	  
	%This case happens if the connection to the client is lost
	{tcp_closed, _Socket} -> 
	  shut_down;
	
	%This case happens when a message has been sent in a diagram and the info is supposed to be sent to the client
	{message_sent, Did, From, To, Message, Message_number} -> 
	  %Turns the result into binary
	  %The character ~ is used as the stop character for when the client should stop reading from the tcp connection
	  Format_result = io_lib:format("~p", [{Did, From, To, Message, Message_number}]) ++ "~",
      %Sends it to the client
	  gen_tcp:send(Socket, [Format_result]),
	  loop(Socket, Diagrams);
	
	%This case happens when there is no more messages in the diagram and the user tries to simulate the next message
	{simulation_done, Did, Message_number} -> 
	  %Turns the result into binary
	  Format_result = io_lib:format("~p", [{Did, simulation_finished, Message_number}]),
      %Sends it to the client
	  gen_tcp:send(Socket, [Format_result ++ "~"]),
	  loop(Socket, Diagrams)
  end.
 
%Finds the correct diagram from the given list
find_diagram(_, []) -> not_created;
find_diagram(Diagram_id, [{Diagram_id, Pid} | _]) -> Pid;
find_diagram(Diagram_id, [_| Diagrams])  -> find_diagram(Diagram_id, Diagrams).

%If the message is the Diagram id and the atom next_message
use_input({ok, {Did, next_message}}, Socket, Diagrams) ->
  %Checks if the diagram exists
  case find_diagram(Did, Diagrams) of
	not_created -> not_ok;
	Pid         -> 
	  %Makes the server simulate the next message
	  Pid ! {next_message, self()},
	  %Confirmation that the diagram coordinator received the message
	  receive
		ok -> ok
	  end,
	  %Receives the result from the diagram coordinator and sends it to the client
	  loop(Socket, Diagrams)
  end;
  
%This pattern will match when the first argument is in the format of a new diagram
use_input({ok, {Did, Class_names, Classes, Messages}}, Socket, Diagrams) ->
  %Spawns a diagram coordinator for this diagram if it doesnt exist already 
  case find_diagram(Did, Diagrams) of 
    not_created -> 
	  %Sends the class names and messages to the client
      %The character ~ is used as the stop character for when the client should stop reading from the tcp connection
      Format_result = io_lib:format("~p", [{Did, Class_names}]) ++ "~",
      gen_tcp:send(Socket, Format_result),
	  Self = self(),
	  loop(Socket, [{Did, spawn(fun () -> diagramcoordinator:init(Self, Did, {Classes, Messages}) end)}| Diagrams]);
	_           -> already_created
  end.