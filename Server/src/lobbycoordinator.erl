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
    {Creator_Socket, create_lobby, Password} -> 
	  Pid = spawn(fun () -> lobby:init(Creator_Socket, Lobby_increment) end),
      Ref = monitor(process, Pid),
	  io:format("Lobby created"),
	  %Format_result = io_lib:format("~p", [{lobby_created, "Lobby has been created"}]),
	  %gen_tcp:send(Creator_Socket, [Format_result]),
	  loop([{Lobby_increment, Password, Pid, Creator_Socket, Ref}|Rooms], Lobby_increment + 1);
	  
	{Creator_Socket, remove_lobby} -> 
	  io:format("before sending ~n"),
	  Lid = [Pid||{_Lid, _Password, Pid, Socket, _Ref} <- Rooms, Creator_Socket == Socket],
	  RefTmp = [Ref || {_Lid, _Password, _Pid, Socket, Ref} <- Rooms, Creator_Socket == Socket],
	  [Ref | _] = RefTmp,
	  [Lobby_PID|_] = Lid,
	  NewRooms = [ {Lid, Password, Pid, Socket, Ref}|| {Lid, Password, Pid, Socket, Ref} <- Rooms, Creator_Socket /= Socket],
	  demonitor(Ref),
	  Lobby_PID ! {remove_lobby, Creator_Socket},
	  io:format("after sending remove ~n"),
	  loop(NewRooms, Lobby_increment);
	  
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