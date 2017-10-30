-module(node).
-export([init/1]).

%%Author: Tim Jonasson
%%Version: 1.2

%Initialized the process for the node
init(Coordinator) ->
  spawn(fun () ->  loop(Coordinator) end).

loop(Coordinator) ->
  receive
    %Case for when the node sends a message for another node
	{send_message, From, To, Message, To_pid, Message_number} ->
	  To_pid ! {receive_message, From, To, Message, Message_number},
	  %Line bellow changed from "Coordinator ! {send_reply},"
	  Coordinator ! {send_reply, From, To, Message, To_pid, Message_number},
      loop(Coordinator);
    
	%Case for when the node receives a message from another node 
	{receive_message, From, To, Message, Message_number} ->
      Coordinator ! {message_done, From, To, Message, Message_number},
      loop(Coordinator)
	  
	after 60000 -> 
	  io:format("~nTerminating the process~p", [self()])
  end.
