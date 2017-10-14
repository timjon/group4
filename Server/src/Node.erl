-module(node).

-export([init/1]).

init(Coordinator) ->
  P = spawn(fun () ->  loop(Coordinator) end),
  io:format("started ~p~n",[P]),
  P.

loop(Coordinator) ->
io:format("New loop  ~n"),
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
  after 600000 ->
    io:format("terminating ~n")
  end.
