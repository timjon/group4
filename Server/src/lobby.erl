-module(lobby).
-export([init/1]).
%%Version: 0.1
%%Collaborators: Sebastian Fransson

init(Creator_Socket) -> loop(Creator_Socket, [Creator_Socket], [], lobby0).


loop(Creator_Socket, Members, Diagrams, Lobby_ID) -> 
  io:format("lobby loop 1~n"),
  receive
    {remove_lobby, Creator_Socket} -> ok;
	
	{join_lobby, Socket} -> 
	  loop(Creator_Socket, case find_member(Members, Socket) of 
	    not_found -> 
		  gen_tcp:send(Socket, io_lib:format("Successfully joined lobby, ~p", [Lobby_ID]) ++ "~"),
		  send_classes(Socket, Diagrams),
		  [Socket|Members];
		found_member -> 
		  gen_tcp:send(Socket, io_lib:format("Already in lobby, ~p", [Lobby_ID]) ++ "~"),
		  Members
		end, Diagrams, Lobby_ID);

	{create_diagram, Creator_Socket, {Did, Class_names, Classes, Messages}} -> 
	  %Sends the class names and messages to the client
      %The character ~ is used as the stop character for when the client should stop reading from the tcp connection
      Diagram_ID = atom_to_list(Lobby_ID) ++ "d" ++ integer_to_list(Did),
	  Format_result = io_lib:format("~p", [{Diagram_ID, Class_names}]) ++ "~",
      send_messages(Members, Format_result),
	  Self = self(),
	  loop(Creator_Socket, Members, [{Diagram_ID, Class_names, spawn(fun () -> diagramcoordinator:init(Self, Diagram_ID, {Classes, Messages}) end)}| Diagrams], Lobby_ID);
	
	{command, Creator_Socket, {Did, Message_request}} -> 
	  Pid = find_diagram(Did, Diagrams),
	  %Sends the message to the diagram coordinator.
	  Pid ! {Message_request, self()},
	  %Confirmation that the diagram coordinator received the message
	  receive
	  	ok -> ok
	  end,
	  %Receives the result from the diagram coordinator and sends it to the client
	  loop(Creator_Socket, Members, Diagrams, Lobby_ID);
	  
	%This case happens when a previous message has been readded to the "queue" or if there were to previous messages to step back to.
	{previous_confirmation, Did, Message} -> 
	  %Turns the message we want to send into binary.
	  Format_result = io_lib:format("~p", [{Did, previous_confirmation, Message}]),
	  %Sends it to the client
	  send_messages(Members, [Format_result ++ "~"]),
	  loop(Creator_Socket, Members, Diagrams, Lobby_ID);
	
	%This case happens when there is no more messages in the diagram and the user tries to simulate the next message
	{simulation_done, Did, Message_number} -> 
	  %Turns the result into binary
	  Format_result = io_lib:format("~p", [{Did, simulation_finished, Message_number}]),
      %Sends it to the client
	  send_messages(Members, [Format_result ++ "~"]),
	  loop(Creator_Socket, Members, Diagrams, Lobby_ID);
	  
	%This case happens when a message has been sent in a diagram and the info is supposed to be sent to the client
	{message_sent, Did, From, To, Message, Message_number} -> 
	  %Turns the result into binary
	  %The character ~ is used as the stop character for when the client should stop reading from the tcp connection
	  Format_result = io_lib:format("~p", [{Did, From, To, Message, Message_number}]) ++ "~",
      %Sends it to the client
	  send_messages(Members, [Format_result]),
	  loop(Creator_Socket, Members, Diagrams, Lobby_ID);
	  
	%This case happens when there are messages to print to the execution log
	{Did, print_information, Msg}->
	  %Turns the message into binary
	  Format_result = io_lib:format("~p", [{Did, print_information, Msg}]),
      %Sends the result to client
	  send_messages(Members, [Format_result ++ "~"]),
	  loop(Creator_Socket, Members, Diagrams, Lobby_ID)
  end.
  
send_classes(_, []) -> done;
send_classes(Socket, [{Diagram_ID, Class_names, _}|Diagrams]) -> 
  Format_result = io_lib:format("~p", [{Diagram_ID, Class_names}]) ++ "~",
  gen_tcp:send(Socket, Format_result),
  send_classes(Socket, Diagrams).
  
%Finds the correct diagram from the given list
find_diagram(_, []) -> not_created;
find_diagram(Diagram_id,[{Diagram_id, _ ,Pid} | _]) -> Pid;
find_diagram(Diagram_id,[_| Diagrams])  -> find_diagram(Diagram_id, Diagrams).  
  
find_member([], _) -> not_found;
find_member([Member|_], Member) -> found_member;
find_member([_|Members], Member) -> find_member(Members, Member).

send_messages(Members, Message) -> [gen_tcp:send(Socket, Message) || Socket <- Members].