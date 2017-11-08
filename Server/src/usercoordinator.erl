-module(usercoordinator).
-export([init/1]).

%%Author: Tim Jonasson
%%Collaborators: Isabelle Törnqvist 2017-10-30, Sebastian Fransson 2017-11-06
%%Version: 2.3

%Initializes the usercoordinator
init(Socket) -> 
  inet:setopts(Socket, [{active, true}]),
  loop(Socket, [], []).

  
loop(Socket, Diagrams, PrevList) -> 
  receive
    {tcp, Socket, Info} -> 
	  %Scans the info read from the tcp connection
	  {ok, Scanned, _} = erl_scan:string(binary_to_list(Info)),
	  use_input(erl_parse:parse_term(Scanned ++ [{dot,0}]), Socket, Diagrams, PrevList);
	  
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
	  loop(Socket, Diagrams, PrevList);
	  
	%This case happens when a previous message has been readded to the "queue" or if there were to previous messages to step back to.
	{previous_confirmation, Did, Message} -> 
	  %Turns the message we want to send into binary.
	  Format_result = io_lib:format("~p", [{Did, previous_confirmation, Message}]) ++ "~",
	  %Sends it to the client
	  gen_tcp:send(Socket, [Format_result ++ "~"]),
	  loop(Socket, Diagram, PrevList);
	
	%This case happens when there is no more messages in the diagram and the user tries to simulate the next message
	{simulation_done, Did, Message_number} -> 
	  %Turns the result into binary
	  Format_result = io_lib:format("~p", [{Did, simulation_finished, Message_number}]),
      %Sends it to the client
	  gen_tcp:send(Socket, [Format_result ++ "~"]),
	  loop(Socket, Diagrams, PrevList);
	  
	%This case happens when there are messages to print to the execution log
	{Did, print_information, Msg}->
		%Turns the message into binary
		Format_result = io_lib:format("~p", [{Did, print_information, Msg}]),
		%Sends the result to client
		gen_tcp:send(Socket, [Format_result ++ "~"]),
		loop(Socket, Diagrams, PrevList)
	  
  end.
 
%Finds the correct diagram from the given list
find_diagram(_, []) -> not_created;
find_diagram(Diagram_id, [{Diagram_id, Pid} | _]) -> Pid;
find_diagram(Diagram_id, [_| Diagrams])  -> find_diagram(Diagram_id, Diagrams).

%If the message is the Diagram id and the atom next_message
use_input({ok, {Did, Message_request}}, Socket, Diagrams, _PrevList) ->
  %Checks if the diagram exists
  case find_diagram(Did, Diagrams) of
	not_created -> not_ok;
	Pid         -> 
	  %Makes the server simulate the next message
	  Pid ! {Message_request, self()},
	  %Confirmation that the diagram coordinator received the message
	  receive
		ok -> ok
	  end,
	  %Receives the result from the diagram coordinator and sends it to the client
	  loop(Socket, Diagrams, _PrevList)
  end;
  
%This pattern will match when the first argument is in the format of a new diagram
use_input({ok, {Did, Class_names, Classes, Messages}}, Socket, Diagrams, _PrevList) ->
  %Spawns a diagram coordinator for this diagram if it doesnt exist already 
  case find_diagram(Did, Diagrams) of 
    not_created -> 
	  %Sends the class names and messages to the client
      %The character ~ is used as the stop character for when the client should stop reading from the tcp connection
      Format_result = io_lib:format("~p", [{Did, Class_names}]) ++ "~",
      gen_tcp:send(Socket, Format_result),
	  Self = self(),
	  loop(Socket, [{Did, spawn(fun () -> diagramcoordinator:init(Self, Did, {Classes, Messages}, _PrevList) end)}| Diagrams], _PrevList);
	_           -> already_created
  end.