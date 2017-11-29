-module(lobby).
-export([init/3]).
%%Version: 1.0
%%Authors: Sebastian Fransson, Tim Jonasson
%%This module manages a lobby and all the diagrams connected to it

init(Creator_Socket, Password, Lobby_ID) -> loop(Creator_Socket, Password, [Creator_Socket], [], Lobby_ID).

loop(Creator_Socket, Password, Members, Diagrams, Lobby_ID) -> 
  receive
    % This happens when a user leaves a lobby.
    {leave_lobby, Socket} -> 
	  % Remove the user from the list and send the remaining users into the loop.
	  NewMembers = [ New || New <- Members, New /= Socket],
	  loop(Creator_Socket, Password, NewMembers, Diagrams, Lobby_ID);
  
    {remove_lobby, Creator_Socket} -> ok; % end the lobby looping to let it terminate naturally.

	%This happens when someone tries to join a lobby
	{join_lobby, Socket, Pwd} -> 
	  %Checks if the user already exists in the lobby list
	  loop(Creator_Socket, Password, case find_member(Members, Socket) of 
	    not_found -> 
		  %Checks if the correct password was given
		  case Pwd of 
		    Password -> 
		      %Sends a confirmation message to the client and then adds the user to the members list
			  gen_tcp:send(Socket, io_lib:format("INFO# Successfully joined lobby, ~p", [Lobby_ID]) ++ "~"),
		      send_classes(Socket, Diagrams),
		      [Socket|Members];
			_ -> 
			  %Sends a message to the client telling the user that the password was incorrect
			  gen_tcp:send(Socket, io_lib:format("INFO# Wrong password to lobby, ~p", [Lobby_ID]) ++ "~"),
			  Members
		  end;
		found_member -> 
		  %Tells the user that they already joined this lobby
		  gen_tcp:send(Socket, io_lib:format("INFO# Already in lobby, ~p", [Lobby_ID]) ++ "~"),
		  Members
		end, Diagrams, Lobby_ID);

	%This happens when the message received contains a sequence diagram
	{create_diagram, Creator_Socket, {Did, Class_names, Classes, Messages}} -> 
	  %Sends the class names and messages to the client
      %The character ~ is used as the stop character for when the client should stop reading from the tcp connection
      Diagram_ID = "l" ++ integer_to_list(Lobby_ID) ++ "d" ++ integer_to_list(Did),
	  Format_result = io_lib:format("~p", [{Diagram_ID, Class_names}]) ++ "~",
      send_messages(Members, Format_result),
	  Self = self(),
	  loop(Creator_Socket, Password, Members, [{Diagram_ID, Class_names, spawn(fun () -> diagramcoordinator:init(Self, Diagram_ID, {Classes, Messages}) end)}| Diagrams], Lobby_ID);
	
	%This case happens when the message recieved contains a command for simulating the diagram
	{command, Creator_Socket, {Did, Message_request}} -> 
	  Pid = find_diagram(Did, Diagrams),
	  %Sends the message to the diagram coordinator.
	  Pid ! {Message_request, self()},
	  %Confirmation that the diagram coordinator received the message
	  receive
	  	ok -> ok
	  end,
	  %Receives the result from the diagram coordinator and sends it to the client
	  loop(Creator_Socket, Password, Members, Diagrams, Lobby_ID);
	  
	%This case happens when a previous message has been readded to the "queue" or if there were to previous messages to step back to.
	{previous_confirmation, Did, Message} -> 
	  %Turns the message we want to send into binary.
	  Format_result = io_lib:format("~p", [{Did, previous_confirmation, Message}]),
	  %Sends it to the client
	  send_messages(Members, [Format_result ++ "~"]),
	  loop(Creator_Socket, Password, Members, Diagrams, Lobby_ID);
	
	%This case happens when there is no more messages in the diagram and the user tries to simulate the next message
	{simulation_done, Did, Message_number} -> 
	  %Turns the result into binary
	  Format_result = io_lib:format("~p", [{Did, simulation_finished, Message_number}]),
      %Sends it to the client
	  send_messages(Members, [Format_result ++ "~"]),
	  loop(Creator_Socket, Password, Members, Diagrams, Lobby_ID);
	  
	%This case happens when a message has been sent in a diagram and the info is supposed to be sent to the client
	{message_sent, Did, From, To, Message, Message_number} -> 
	  %Turns the result into binary
	  %The character ~ is used as the stop character for when the client should stop reading from the tcp connection
	  Format_result = io_lib:format("~p", [{Did, From, To, Message, Message_number}]) ++ "~",
      %Sends it to the client
	  send_messages(Members, [Format_result]),
	  loop(Creator_Socket, Password, Members, Diagrams, Lobby_ID);
	  
	%This case happens when there are messages to print to the execution log
	{Did, print_information, Msg}->
	  %Turns the message into binary
	  Format_result = io_lib:format("~p", [{Did, print_information, Msg}]),
      %Sends the result to client
	  send_messages(Members, [Format_result ++ "~"]),
	  loop(Creator_Socket, Password, Members, Diagrams, Lobby_ID)
  end.
  
%Sends the classes of all given diagrams to the given user
send_classes(_, []) -> done;
send_classes(Socket, [{Diagram_ID, Class_names, _}|Diagrams]) -> 
  Format_result = io_lib:format("~p", [{Diagram_ID, Class_names}]) ++ "~",
  gen_tcp:send(Socket, Format_result),
  send_classes(Socket, Diagrams).
  
%Finds the correct diagram from the given list
find_diagram(_, []) -> not_created;
find_diagram(Diagram_id,[{Diagram_id, _ ,Pid} | _]) -> Pid;
find_diagram(Diagram_id,[_| Diagrams])  -> find_diagram(Diagram_id, Diagrams).  
  
%Finds the given member in the given list of members
find_member([], _) -> not_found;
find_member([Member|_], Member) -> found_member;
find_member([_|Members], Member) -> find_member(Members, Member).

%Sends a message to all the users in the given list
send_messages(Members, Message) -> [gen_tcp:send(Socket, Message) || Socket <- Members].