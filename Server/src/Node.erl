-module(node).

-export([init/2]).

init(Coordinator, Class_name) ->
  P = spawn_link(fun () ->  loop(Coordinator) end),
  io:format("started ~p~n",[P]),
  register(Class_name, P),
  erlang:monitor(process, P).

loop(Coordinator) ->
io:format("New loop  ~n"),
  receive
    {send_message} ->
      Coordinator ! {send_reply},
      loop(Coordinator);
    {receive_message} ->
      Coordinator ! {receive_reply},
      loop(Coordinator);
	{crash_message} ->
	  io:format("Brb crashing~n"),
      exit(self(), plz_die),
	  io:format("Brb crashing again~n"),
	  loop(Coordinator)
  end.
