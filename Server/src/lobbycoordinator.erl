-module(lobbycoordinator).
-export([init/0, remove_lobby/2]).
%%Version 0.1
%%Collaborators: Sebastian Fransson

%Initializes the lobby
init() -> 
  loop([], 0).
  
%the loop keeps track of the rooms and handles lobby requests.
loop(Rooms, Lobby_increment) ->
  receive 
    {Creator_Socket, create_lobby} -> 
	  Pid = spawn(fun () -> lobby:init(Creator_Socket) end),
      monitor(process, Pid),
	  io:format("Lobby created"),
	  loop([{Lobby_increment, Pid}|Rooms], Lobby_increment + 1);
	  
	{remove_lobby, Creator_Socket, Lobby_ID} -> 
	  Lobby_ID ! {remove_lobby, Creator_Socket},
	  demonitor(Lobby_ID),
	  loop(remove_lobby(Lobby_ID, Rooms), Lobby_increment);
	  
    {join_lobby, Socket, Lobby_ID} -> not_implemented;
	  
	{leave_lobby, Socket, Lobby_ID} -> not_implemented;
	
	{Creator_Socket, {command, Command}} -> 
	 io:format("hello"), not_implemented;

	{'DOWN', _Ref, _process, Pid, Reason} -> 
	  %Send a notice to the Client stating that a lobbt has crashed as well as relay the reason.
	  Format_result = io_lib:format("~p", [{Pid, lobby_crashed, Reason}])
  end.
  
find_room([], Lobby_ID)                  -> not_created ;
find_room([{Lobby_ID, Pid}|Ls], Lobby_ID) -> Pid;
find_room([_|Ls], Lobby_ID)               -> find_room(Ls, Lobby_ID).

remove_lobby(Lobby_ID, []) -> [];
remove_lobby(Lobby_ID, [{Lobby_ID, Password, Pid}|Lobbys]) -> Lobbys;
remove_lobby(Lobby_ID, [Lobby|Lobbys]) -> [Lobby|remove_lobby(Lobby_ID, Lobbys)].