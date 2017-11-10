-module(lobby).
-export([init/1]).

init(Creator_Socket) -> loop(Creator_Socket, [Creator_Socket], [], lobby1).


loop(Creator_Socket, Members, Diagrams, Lobby_ID) -> 
  receive
    {remove_lobby, Creator_Socket} -> ok;
	
	{create_diagram, Creator_Socket, {ok, {Did, Class_names, Classes, Messages}}} -> 
	  %Sends the class names and messages to the client
      %The character ~ is used as the stop character for when the client should stop reading from the tcp connection
      Diagram_ID = Did ++ Lobby_ID,
	  Format_result = io_lib:format("~p", [{Diagram_ID, Class_names}]) ++ "~",
      gen_tcp:send(Creator_Socket, Format_result),
	  Self = self(),
	  loop(Creator_Socket, Members, [{Diagram_ID, spawn(fun () -> diagramcoordinator:init(Self, Diagram_ID, {Classes, Messages}) end)}| Diagrams], Lobby_ID);
	
	{command, Creator_Socket, {ok, {Did, Message_request}}} -> 
	  Pid = usercoordinator:find_diagram(Did, Diagrams),
	  %Sends the message to the diagram coordinator.
	  Pid ! {Message_request, self()},
	  %Confirmation that the diagram coordinator received the message
	  receive
	  	ok -> ok
	  end,
	  %Receives the result from the diagram coordinator and sends it to the client
	  loop(Creator_Socket, Members, Diagrams, Lobby_ID);
    {remove_diagram, Did} -> not_implemented
  end.
  