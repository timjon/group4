-module(node).
-export([init/1]).

%%Author: Tim Jonasson
%%Version: 1.1

%Initialized the process for the node
init(Coordinator) ->
  spawn(fun () ->  loop(Coordinator) end).

loop(Coordinator) ->
  receive
    %Case for when the node sends a message for another node
	{send_message, From, To, Message, To_pid} ->
	  To_pid ! {receive_message, From, To, Message},
	  Coordinator ! {send_reply},
      loop(Coordinator);
    
	%Case for when the node receives a message from another node
	{receive_message, From, To, Message} ->
      Coordinator ! {message_done, From, To, Message},
      loop(Coordinator)
	  
	after 60000 -> 
	  io:format("~nTerminating the process~p", [self()])
  end.