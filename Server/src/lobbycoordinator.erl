-module(lobbycoordinator).
-export([init/0, remove_lobby/2, get_lobby_ID/1]).
%%Version 0.8
%%Collaborators: Sebastian Fransson

%Initializes the lobby
init() -> 
  loop([], 0).
  
%the loop keeps track of the rooms and handles lobby requests.
loop(Rooms, Lobby_increment) ->
  io:format("hhello ~n~p", [Lobby_increment]),
  receive 
    %Creates a diagram and saves the monitor reference as well as lobby info to the list of rooms.
    {Creator_Socket, create_lobby, Password} -> 
	  Pid = spawn(fun () -> lobby:init(Creator_Socket, Password, Lobby_increment) end),
      Ref = monitor(process, Pid),
	  io:format("Lobby created"),
	  gen_tcp:send(Creator_Socket, io_lib:format("INFO# Created lobby with id:,  ~p", [Lobby_increment]) ++ "~"),
	  loop([{Lobby_increment, Password, Pid, Creator_Socket, Ref}|Rooms], Lobby_increment + 1);
	
	%Demonitors, kills and removes the host's lobby from the list of rooms.
	{Creator_Socket, remove_lobby} -> 
	  io:format("before sending ~n"),
	  %Finds the lobby id and Pid of the lobby process.
	  LPid = [Pid||{_Lid, _Password, Pid, Socket, _Ref} <- Rooms, Creator_Socket == Socket],
	  RefTmp = [Ref || {_Lid, _Password, _Pid, Socket, Ref} <- Rooms, Creator_Socket == Socket],
	  [Ref | _] = RefTmp,
	  [Lobby_PID|_] = LPid,
	  NewRooms = [ {Lid, Password, Pid, Socket, _Ref}|| {Lid, Password, Pid, Socket, _Ref} <- Rooms, Creator_Socket /= Socket],
	  %demonitor the lobby to avoid receiving the 'DOWN' message, since we manually terminated the process.
	  demonitor(Ref),
	  %Send a confirmation to the lobby so that it exits naturally.
	  Lobby_PID ! {remove_lobby, Creator_Socket},
	  gen_tcp:send(Creator_Socket, io_lib:format("INFO# Successfully removed the lobby at PID: ~p", [LPid]) ++ "~"),
	  io:format("after sending remove ~n"),
	  loop(NewRooms, Lobby_increment);
	  
	%This happens when a user tries to join a lobby
    {Socket, {join_lobby, {Lobby_ID, Password}}} -> 
	  %Finds the room the user wants to find
	  case find_room(Rooms, Lobby_ID) of 
	    not_created -> not_created;
		Pid         ->  Pid ! {join_lobby, Socket, Password}, io:format("created")
	  end,
	  loop(Rooms, Lobby_increment);
	  
	{User_Socket, leave_lobby, Lobby_ID} -> 
	  LPid = hd([Pid||{Lid, _Password, Pid, _Socket, _Ref} <- Rooms, Lobby_ID == Lid]),
	  LPid ! {leave_lobby, User_Socket},
	  gen_tcp:send(User_Socket, io_lib:format("INFO# Successfully left the lobby at PID: ~p", [LPid]) ++ "~"),
	  loop(Rooms, Lobby_increment);
	  
	
	{Creator_Socket, {Did, Class_names, Classes, Messages}} -> 
	  io:format("wow~n"),
	  Lid = [Lid||{Lid, _Password, _Pid, Socket, _Ref} <- Rooms, Creator_Socket == Socket],
	  io:format("wow~n~p", [Lid]),
	  case Lid of 
	    [] -> no_lobby_created;
	    [Head|_] -> 
		  case find_room(Rooms, Head) of
	        not_created -> no_lobby_created;
		    Pid         -> Pid ! {create_diagram, Creator_Socket, {Did, Class_names, Classes, Messages}}
		  end
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
	  Format_result = io_lib:format("~p", [{Pid, lobby_crashed, Reason}]);
	 _ -> 
		loop(Rooms, Lobby_increment)
  end.
  
find_room([], _Lobby_ID)                             -> not_created ;
find_room([{Lobby_ID, _Password, Pid, _, _}|_Ls], Lobby_ID) -> Pid;
find_room([_L|Ls], Lobby_ID)                         -> find_room(Ls, Lobby_ID).

get_lobby_ID([]) -> [];
get_lobby_ID([L|Ls]) when (L >= $0) and (L =< $9) -> get_ID([L|Ls]);
get_lobby_ID([_|Ls]) -> get_lobby_ID(Ls).

get_ID([]) -> [];
get_ID([L|Ls]) when (L >= $0) and (L =< $9) -> [L| get_ID(Ls)];
get_ID(_) -> [].

remove_lobby(_Lobby_ID, []) -> [];
remove_lobby(Lobby_ID, [{Lobby_ID, _Password, _Pid}|Lobbys]) -> Lobbys;
remove_lobby(Lobby_ID, [Lobby|Lobbys]) -> [Lobby|remove_lobby(Lobby_ID, Lobbys)].