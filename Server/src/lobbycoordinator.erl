-module(lobbycoordinator).
-export([init/0]).
%%Version 0.1
%%Collaborators: Sebastian Fransson

%Initializes the lobby
init() -> 
  loop([], 0).
  
%the loop keeps track of the rooms and handles lobby requests.
loop(Rooms, Lobby_increment) ->
  receive 
    {create_lobby, Creator_Socket, Password} -> 
	  Pid = spawn(fun () -> lobby:init(Creator_Socket, Password, self()) end),
      monitor(Pid),
	  Rooms ++ [{Lobby_increment, Password, Pid}],
	  created_lobby,
	  loop(Rooms, Lobby_increment + 1)
	  
	{remove_lobby, Creator_Socket, Lobby_ID} -> 
	  Lobby_ID ! {remove_lobby, Creator_Socket},
	  demonitor(Lobby_ID);
	  
    {join_lobby, Socket, Lobby_ID} -> not_implemented;
	  
	{leave_lobby, Socket, Lobby_ID} -> not_implemented;
	
	{command, Creator_Socket, Lobby_ID, Command} -> not_implemented;

	{'DOWN', _Ref, _process, Pid, Reason} -> 
	  Format_result = io_lib:format("~p", [{Lid, lobby_crashed, Reason}]), 
      loop(Rooms, counter)
  end.
  
find_room([], Lobby_ID)                  -> not_created ;
find_room([{Lobby_ID, Pid}|Ls], Lobby_ID) -> Pid;
find_room([_|Ls], Lobby_ID)               -> find_room(Ls, Lobby_ID).
