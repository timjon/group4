-module(node).
-export([init/1]).

%%Author: Tim Jonasson
%%Collaborators: Pontus Laestadius 2017-12-04
%%Version: 1.4

%Initialized the process for the node
init(Coordinator) ->
  spawn(fun () ->  loop(Coordinator, none, none) end).

loop(Coordinator, Name, Fields) ->
  receive
  
  % Gets the name of the node.
  {Pid, getName} -> 
  	Pid ! {getName, Name},
  	loop(Coordinator, Name, Fields);
  	
  % Sets the name of the node.
  {setName, NewName} -> 
  	loop(Coordinator, NewName, Fields);
  	
  	% Gets the name of the node.
  {Pid, getFields} -> 
  	Pid ! {getFields, Fields},
  	loop(Coordinator, Name, Fields);
  	
  % Sets the name of the node.
  {setFields, NewFields} -> 
  	loop(Coordinator, Name, NewFields);
  
    %Case for when the node sends a message for another node
	{send_message, From, To, Message, To_pid, Message_number} ->
	  To_pid ! {receive_message, From, To, Message, Message_number},
	  %Line bellow changed from "Coordinator ! {send_reply},"
	  Coordinator ! {send_reply, From, To, Message, To_pid, Message_number},
      loop(Coordinator, Name, Fields);
    
	%Case for when the node receives a message from another node 
	{receive_message, From, To, Message, Message_number} ->
      Coordinator ! {message_done, From, To, Message, Message_number},
      loop(Coordinator, Name, Fields)
  end.
  
