-module(lobbymonitor).
-export([init/0]).

init() -> 
  Pid = spawn(fun() -> lobbycoordinator:init() end),
  monitor(process,Pid),
  register(lobbycoordinator, Pid),
  loop(Pid).
  
  loop(Pid) ->
    receive
      {'DOWN', _Ref, _process, Pid, _Reason} -> 
	    NewPid = spawn(fun() -> lobbycoordinator:init() end),
		monitor(process, NewPid),
		register(lobbycoordinator, NewPid),
		loop(NewPid)
    end.
  
