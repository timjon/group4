-module(node).
-export([init/1]).

%%Author: Tim Jonasson
%%Version: 1.3

%Initialized the process for the node
init(Coordinator) ->
  spawn(fun () ->  loop(Coordinator) end).

loop(Coordinator) ->
  receive
    %Case for when the node sends a message for another node
	{send_message, From, To, Message, Message_number} ->
	  Coordinator ! {send_reply, From, To, Message, Message_number},
      loop(Coordinator);
    
	%Case for when the node receives a message from another node 
	{receive_message, From, To, Message, Message_number} ->
      Coordinator ! {message_done, From, To, Message, Message_number},
      loop(Coordinator)
  end.
