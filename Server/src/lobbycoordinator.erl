-module(lobbycoordinator).
-export([init/0]).

%Initializes the lobby
init() -> 
  loop([], 0).
  
loop(Rooms, Counter) ->
  receive 
    {create_lobby, Creator_Socket} -> 
	  %%TODO - Monitor
	  spawn(fun () -> lobby:init(Creator_Socket) end),
      loop(Rooms, counter);
	  
	{remove_lobby, Creator_Socket, Lobby_ID} -> 
	  Lobby_ID ! {remove_lobby, Creator_Socket},
      loop(Rooms, counter);
	  
    {join_lobby, Socket, Lobby_ID} -> ok;
	{leave_lobby, Socket, Lobby_ID} -> not_implemented;
	{command, Creator_Socket, Lobby_ID, Command} -> not_implemented;

	{'DOWN', _Ref, _process, Lid, Reason} -> 
	  %Send a notice to the Client stating that a lobbt has crashed as well as relay the reason.
	  Format_result = io_lib:format("~p", [{Lid, lobby_crashed, Reason}]), 
      loop(Rooms, counter)
  end.
  
find_room([], Lobby_ID)                  -> not_created ;
find_room([{Lobby_ID, Pid}|Ls], Lobby_ID) -> Pid;
find_room([_|Ls], Lobby_ID)               -> find_room(Ls, Lobby_ID).
