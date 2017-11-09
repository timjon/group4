-module(lobby).
-export([]).

init(LobbyCoordinator, Lid, Host_id) - >
  LobbyCoordinator ! {Lid, print_information, ["Lobby was spawned"]},
  loop(LobbyCoordinator, Lid, Host_id).

loop(LobbyCoordinator, Lid, Host_id) ->
  receive
    {create_diagram, Diagram, Did} -> ;  %Spawn the DiagramCoordinator, the function 'use_input' from usercoordinator could be used here.
	
	{remove_diagram, Did} -> ;
	
	