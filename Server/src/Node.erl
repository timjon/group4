-module(node).

-export([init/2]).

init(Coordinator, Class_name) ->
  P = spawn_link(fun () ->  loop(Coordinator) end),
  register(Class_name, P).

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
      error(server_fault)
  end.
