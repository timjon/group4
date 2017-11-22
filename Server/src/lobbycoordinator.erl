-module(lobbycoordinator).
-export([init/0, remove_lobby/2, get_lobby_ID/1]).
%%Version 0.1
%%Collaborators: Sebastian Fransson

%Initializes the lobby
init() -> 
  loop([], 0).
  
%the loop keeps track of the rooms and handles lobby requests.
loop(Rooms, Lobby_increment) ->
  io:format("hhello ~n~p", [Lobby_increment]),
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
	  %demonitor the lobby to avoid receiving the 'DOWN' message, since we manually terminated the process.
	  demonitor(Ref),
	  Lobby_PID ! {remove_lobby, Creator_Socket},
	  io:format("after sending remove ~n"),
	  loop(NewRooms, Lobby_increment);
	  
    {Socket, {join_lobby, {Lobby_ID, Password}}} -> 
	  find_room(Rooms, Lobby_ID) ! {join_lobby, Socket},
	  loop(Rooms, Lobby_increment);
	  
	{leave_lobby, Socket, Lobby_ID} -> not_implemented;
	
	{Creator_Socket, {Did, Class_names, Classes, Messages}} -> 
	  io:format("wow~n"),
	  Lid = hd([Lid||{Lid, _Password, _Pid, Socket, _Ref} <- Rooms, Creator_Socket == Socket]),
	  io:format("wow~n~p", [Lid]),
	  case find_room(Rooms, Lid) of
	    not_created -> no_lobby_created;
		Pid         -> Pid ! {create_diagram, Creator_Socket, {Did, Class_names, Classes, Messages}}
	  end,
	  loop(Rooms, Lobby_increment);
	
	{Creator_Socket, {Did, Command}} -> 
	  case find_room(Rooms, list_to_integer(get_lobby_ID(Did))) of 
	    not_created -> no_lobby_created,
		  io:format("no lobby");
		Pid         -> Pid ! {command, Creator_Socket, {Did, Command}},
		  io:format("Sent: ~n~p", [{command, {Did, Command}}])
	  end,
	  loop(Rooms, Lobby_increment);

	{'DOWN', _Ref, _process, Pid, Reason} -> 
	  %Send a notice to the Client stating that a lobbt has crashed as well as relay the reason.
	  Format_result = io_lib:format("~p", [{Pid, lobby_crashed, Reason}])
  end.
  
find_room([], Lobby_ID)                             -> not_created ;
find_room([{Lobby_ID, Password, Pid, _, _}|Ls], Lobby_ID) -> Pid;
find_room([L|Ls], Lobby_ID)                         -> find_room(Ls, Lobby_ID).

get_lobby_ID([]) -> [];
get_lobby_ID([L|Ls]) when (L >= $0) and (L =< $9) -> get_ID([L|Ls]);
get_lobby_ID([_|Ls]) -> get_lobby_ID(Ls).

get_ID([]) -> [];
get_ID([L|Ls]) when (L >= $0) and (L =< $9) -> [L| get_ID(Ls)];
get_ID(_) -> [].

remove_lobby(Lobby_ID, []) -> [];
remove_lobby(Lobby_ID, [{Lobby_ID, Password, Pid}|Lobbys]) -> Lobbys;
remove_lobby(Lobby_ID, [Lobby|Lobbys]) -> [Lobby|remove_lobby(Lobby_ID, Lobbys)].