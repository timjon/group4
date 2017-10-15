-module(node).

-export([init/1]).


init(Coordinator) ->
  P = spawn(fun () ->  loop(Coordinator) end),
  io:format("started ~p~n",[P]),
  P.

loop(Coordinator) ->
  receive
    {send_message} ->
      Coordinator ! {send_reply},
      loop(Coordinator);
    {receive_message} ->
      Coordinator ! {receive_reply},
      loop(Coordinator);
	{kill_message} ->
      Coordinator ! {kill_reply},
	  io:format("Killed ~n")
	after 500 -> 
	  io:format("Terminating the process ~p", [self()]),
	  io:format("~n")
  end.
