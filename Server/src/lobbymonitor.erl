-module(lobbymonitor).
-export([init/0]).
%% Version: 1.0
%% Author: Sebastian Fransson

% initializes the loop, spawns the lobbycoordinator, monitors the lobbycoordinator and registers it.
init() -> 
  Pid = spawn(fun() -> lobbycoordinator:init() end),
  monitor(process,Pid),
  register(lobbycoordinator, Pid),
  loop(Pid).
  
  loop(Pid) ->
    receive
	  % if the lobbycoordinator process were to die, this process catches the resulting message and respawns it.
      {'DOWN', _Ref, _process, Pid, _Reason} -> 
	    NewPid = spawn(fun() -> lobbycoordinator:init() end),
		monitor(process, NewPid),
		register(lobbycoordinator, NewPid),
		loop(NewPid)
    end.
  
