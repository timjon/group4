-module(lobbycoordinator).
-export([loop/]).
-import(usercoordinator, [find_diagram/2, use_input/3]).

% Author: Sebastian Fransson

loop(Socket, Diagrams, UserList) ->
  receive
	  
	{create_lobby, Host_id} -> 
	  monitor(process, spawn(fun () -> lobby:init(Self, Lid, Host_id) end));
	
	{join_lobby, User_id, LobbyName } -> ;
	
	{leave_lobby, User_id, LobbyName} -> ;
	
	{remove_lobby, Host_id, LobbyName} -> ;
	
	{send_to_lobby, Message, } -> ;
	
	{'DOWN', _Ref, _process, Lid, Reason} -> 
		%Send a notice to the Client stating that a lobbt has crashed as well as relay the reason.
		Format_result = io_lib:format("~p", [{Lid, lobby_crashed, Reason}])
	
	
  end.
  
  
  