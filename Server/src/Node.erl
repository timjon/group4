-module(node).

-export([init/1]).


init(Coordinator) ->
  P = spawn(fun () ->  loop(Coordinator) end),
  io:format("started ~p~n",[P]),
  P.

loop(Coordinator) ->
  receive
    {send_message, From_pid, From, To, Message, To_pid} ->
	  io:format("is this it? ~n "),
      To_pid ! {receive_message, From, To, Message},
	  io:format("Well fuck~n "),
	  From_pid ! {send_reply},
      loop(Coordinator);
    {receive_message, From, To, Message} ->
      Coordinator ! {message_done, From, To, Message},
      loop(Coordinator);
	{kill_message} ->
      Coordinator ! {kill_reply},
	  io:format("Killed ~n")
	after 50000 -> 
	  io:format("~nTerminating the process~p", [self()])
  end.
