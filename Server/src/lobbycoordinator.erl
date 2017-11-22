-module(lobbycoordinator).
-export([init/0, remove_lobby/2, get_lobby_ID/1]).
%%Version 0.1
%%Collaborators: Sebastian Fransson

%Initializes the lobby
init() -> 
  loop([], 0).
  
%the loop keeps track of the rooms and handles lobby requests.
loop(Rooms, Lobby_increment) ->
  receive 
    {Creator_Socket, Password, create_lobby} -> 
	  Pid = spawn(fun () -> lobby:init(Creator_Socket) end),
      monitor(process, Pid),
	  io:format("Lobby created"),
	  loop([{Lobby_increment, Password, Pid}|Rooms], Lobby_increment + 1);
	  
	{remove_lobby, Creator_Socket, Lobby_ID} -> 
	  Lobby_ID ! {remove_lobby, Creator_Socket},
	  io:format("hello"),
	  demonitor(Lobby_ID),
	  loop(remove_lobby(Lobby_ID, Rooms), Lobby_increment);
	  
    {Socket, {join_lobby, {Lobby_ID, Password}}} -> 
	  find_room(Rooms, Lobby_ID) ! {join_lobby, Socket},
	  loop(Rooms, Lobby_increment);
	  
	{leave_lobby, Socket, Lobby_ID} -> not_implemented;
	
	{Creator_Socket, {Lobby_ID, {Did, Class_names, Classes, Messages}}} -> 
	  io:format("~n ~p", [get_lobby_ID(atom_to_list(Lobby_ID))]),
	  case find_room(Rooms, list_to_integer(get_lobby_ID(atom_to_list(Lobby_ID)))) of
	    not_created -> no_lobby_created;
		Pid         -> Pid ! {create_diagram, Creator_Socket, {Did, Class_names, Classes, Messages}}
	  end,
	  loop(Rooms, Lobby_increment);
	
	{Creator_Socket, {command, Command}} -> 
	 io:format("hello"), not_implemented;

	{'DOWN', _Ref, _process, Pid, Reason} -> 
	  %Send a notice to the Client stating that a lobbt has crashed as well as relay the reason.
	  Format_result = io_lib:format("~p", [{Pid, lobby_crashed, Reason}])
  end.
  
find_room([], Lobby_ID)                  -> not_created ;
find_room([{Lobby_ID, Password, Pid}|Ls], Lobby_ID) -> Pid;
find_room([_|Ls], Lobby_ID)               -> find_room(Ls, Lobby_ID).

get_lobby_ID([]) -> [];
get_lobby_ID([L|Ls]) when (L >= $0) and (L =< $9) -> get_ID([L|Ls]);
get_lobby_ID([_|Ls]) -> get_lobby_ID(Ls).

get_ID([]) -> [];
get_ID([L|Ls]) when (L >= $0) and (L =< $9) -> [L| get_ID(Ls)];
get_ID(_) -> [].

remove_lobby(Lobby_ID, []) -> [];
remove_lobby(Lobby_ID, [{Lobby_ID, Password, Pid}|Lobbys]) -> Lobbys;
remove_lobby(Lobby_ID, [Lobby|Lobbys]) -> [Lobby|remove_lobby(Lobby_ID, Lobbys)].