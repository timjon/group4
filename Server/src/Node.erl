-module(node).
-export([init/1]).

%%Author: Tim Jonasson
%%Version: 1

%Initialized the process for the node
init(Coordinator) ->
  spawn(fun () ->  loop(Coordinator) end).

loop(Coordinator) ->
  receive
    %Case for when the node sends a message for another node
	{send_message, From_pid, From, To, Message, To_pid} ->
	  To_pid ! {receive_message, From, To, Message},
	  From_pid ! {send_reply},
      loop(Coordinator);
    
	%Case for when the node is receives a message from another node
	{receive_message, From, To, Message} ->
      Coordinator ! {message_done, From, To, Message},
      loop(Coordinator)
	  
	after 50000 -> 
	  io:format("~nTerminating the process~p", [self()])
  end.
