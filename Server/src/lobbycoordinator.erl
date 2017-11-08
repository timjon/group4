-module(lobbycoordinator).
-export([loop/]).
-import(usercoordinator, [find_diagram/2, use_input/3]).

% Author: Sebastian Fransson

loop(Socket, Diagrams, UserList) ->
  receive
    {tcp, Socket, Info} -> 
	  %Scans the info read from the tcp connection
	  {ok, Scanned, _} = erl_scan:string(binary_to_list(Info)),
	 % use_input(erl_parse:parse_term(Scanned ++ [{dot,0}]), Socket, Diagrams); % need to fix this function to fit the lobby setup.
	
	{tcp_closed, _Socket} ->
	  conncetion_closed;
	  
	{create_lobby, Host_id} -> ;
	
	{join_lobby, User_id, LobbyName } -> ;
	
	{leave_lobby, User_id, LobbyName} -> ;
	
	{remove_lobby, Host_id, LobbyName} -> ;
	
	{send_to_lobby, Message, } ->
	
	
  end.
  
  
  