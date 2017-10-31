-module(node).
-export([init/1]).

%%Author: Tim Jonasson
%%Version: 1.3
%%@collaborator Kosara Golemshinska

%Initialized the process for the node
init(Coordinator) ->
  Pid = spawn(fun () ->  loop(Coordinator) end),
  % Link the current process to the child Node.
  link(Pid),
  Pid.

loop(Coordinator) ->
  receive
    %Case for when the node sends a message for another node
	{send_message, From, To, Message, To_pid, Message_number} ->
	  To_pid ! {receive_message, From, To, Message, Message_number},
	  Coordinator ! {send_reply},
      loop(Coordinator);
    
	%Case for when the node receives a message from another node 
	{receive_message, From, To, Message, Message_number} ->
      Coordinator ! {message_done, From, To, Message, Message_number},
      loop(Coordinator)
	  
	after 60000 -> 
	  io:format("~nTerminating the process~p", [self()])
  end.
