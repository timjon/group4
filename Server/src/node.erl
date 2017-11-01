-module(node).
-export([init/1]).

%%Author: Tim Jonasson
%%Collaborator: Kosara Golemshinska 2017-11-31
%%Version: 1.4

%Initialized the process for the node
init(Coordinator) ->
  % Create a node process and link to it.
  spawn_link(fun () -> loop(Coordinator) end).

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
	  
  end.
