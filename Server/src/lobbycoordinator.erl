-module(lobbycoordinator).
-export([init/0]).
%%Version 1.1
%%Authors: Sebastian Fransson, Tim Jonasson
%%Collaborators: Pontus Laestadius 2017-12-08

%Initializes the lobby
init() -> 
  loop([], 0).
  
%the loop keeps track of the rooms and handles lobby requests.
loop(Rooms, Lobby_increment) ->
  receive 
    %Creates a lobby and saves the monitor reference as well as lobby info to the list of rooms.
    {Creator_Socket, create_lobby, Password} -> 
	  % Spawn the process and save the Pid.
	  Pid = spawn(fun () -> lobby:init(Creator_Socket, Password, Lobby_increment) end),
	  % Save the reference so that we can use it later for demonitoring.
      Ref = monitor(process, Pid),
	  % Send a confimation to the client saying that the lobby was created.
	  gen_tcp:send(Creator_Socket, io_lib:format("INFO# Created lobby with id:,  ~p", [Lobby_increment]) ++ "~"),
	  loop([{Lobby_increment, Password, Pid, Creator_Socket, Ref}|Rooms], Lobby_increment + 1);
	
	%Demonitors, kills and removes the host's lobby from the list of rooms.
	{Creator_Socket, remove_lobby} -> 
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
	  %Send a confirmation to the client that a lobby has been removed.
	  gen_tcp:send(Creator_Socket, io_lib:format("INFO# Successfully removed the lobby at PID:, ~p", [LPid]) ++ "~"),
	  loop(NewRooms, Lobby_increment);
	  
	%This happens when a user tries to join a lobby
    {Socket, {join_lobby, {Lobby_ID, Password}}} -> 
	  %Finds the room the user wants to find
	  case find_room(Rooms, Lobby_ID) of 
	    not_created -> not_created;
		Pid         ->  Pid ! {join_lobby, Socket, Password}
	  end,
	  loop(Rooms, Lobby_increment);
	  
	% This happens when a user tries to leave a lobby.
	{User_Socket, leave_lobby, Lobby_ID} -> 
	  % Check the list and retrieve the lobby PID of the specified lobby.
	  LPid = hd([Pid||{Lid, _Password, Pid, _Socket, _Ref} <- Rooms, Lobby_ID == Lid]),
	  LPid ! {leave_lobby, User_Socket},
	  % Send a confrimation to the client that a lobby has been successfully left.
	  gen_tcp:send(User_Socket, io_lib:format("INFO# Successfully left the lobby at PID:, ~p", [LPid]) ++ "~"),
	  loop(Rooms, Lobby_increment);
	  
	{Creator_Socket, {deployment_diagram, Sid, Did, Mappings}} ->
	  %Get lobby and send deployment through.
	  case find_room(Rooms, list_to_integer(get_lobby_ID(Sid))) of 
	    not_created -> no_lobby_created;
		Pid         -> Pid ! {deployment_diagram, Creator_Socket, {deployment_diagram, Sid, Did, Mappings}}
	  end,
	  loop(Rooms, Lobby_increment);
	  
	%This happens when you send a new diagram to the lobby
	{Creator_Socket, {Did, Class_names, Classes, Messages}} -> 
	  io:format("Im creating stuff ~n"),
	  %Gets the lobby id
	  Lid = [Lid||{Lid, _Password, _Pid, Socket, _Ref} <- Rooms, Creator_Socket == Socket],
	  %Checks if the lobby exists
	  case Lid of 
	    [] -> no_lobby_created;
	    [Head|_] -> 
		  %Gets the pid
		  case find_room(Rooms, Head) of
	        not_created -> no_lobby_created;
		    Pid         -> Pid ! {create_diagram, Creator_Socket, {Did, Class_names, Classes, Messages}}
		  end
	  end,
	  loop(Rooms, Lobby_increment);
	
	%This happens when a command is sent to a shared diagram
	{Creator_Socket, {Did, Command}} -> 
	  %Checks if the lobby exists
	  case find_room(Rooms, list_to_integer(get_lobby_ID(Did))) of 
	    not_created -> no_lobby_created;
		Pid         -> Pid ! {command, Creator_Socket, {Did, Command}}
	  end,
	  loop(Rooms, Lobby_increment);

	{'DOWN', _Ref, _process, _Pid, _Reason} -> 
	   ok,
	   loop(Rooms, Lobby_increment);
	   
	 _ -> 
		loop(Rooms, Lobby_increment)
  end.
  
%Finds the pid related to the given lobby id
find_room([], _Lobby_ID)                             -> not_created ;
find_room([{Lobby_ID, _Password, Pid, _, _}|_Ls], Lobby_ID) -> Pid;
find_room([_L|Ls], Lobby_ID)                         -> find_room(Ls, Lobby_ID).

%Gets the lobby id of the given string
get_lobby_ID([]) -> [];
get_lobby_ID([L|Ls]) when (L >= $0) and (L =< $9) -> get_ID([L|Ls]);
get_lobby_ID([_|Ls]) -> get_lobby_ID(Ls).

%Helper function that takes all numbers until another char appears
get_ID([]) -> [];
get_ID([L|Ls]) when (L >= $0) and (L =< $9) -> [L| get_ID(Ls)];
get_ID(_) -> [].
