-module(node).
-export([init/2]).

%%Author: Tim Jonasson
%%Collaborators: Pontus Laestadius 2017-12-04
%%Version: 1.4

%Initialized the process for the node
init(Coordinator, Name) ->
  spawn(fun () ->  loop(Coordinator, Name, none) end).

loop(Coordinator, Name, Class) ->
  receive
  
  % Gets the name of the node.
  {Pid, getName} -> 
  	Pid ! {getName, Name},
  	loop(Coordinator, Name, Class);
  	
  	% Gets the class of the node.
  {Pid, getClass} -> 
  	Pid ! {getClass, Class},
  	loop(Coordinator, Name, Class);
  	
  % Adds data to the node.
  {setName, NewName} ->
  	loop(Coordinator, NewName, Class);
  	
  	  % Adds data to the node.
  {setClass, NewClass} ->
  	loop(Coordinator, Name, NewClass);
  
    %Case for when the node sends a message for another node
	{send_message, From, To, Message, To_pid, Message_number} ->
	  To_pid ! {receive_message, From, To, Message, Message_number},
	  Coordinator ! {send_reply, From, To, Message, To_pid, Message_number},
  	loop(Coordinator, Name, Class);
    
	%Case for when the node receives a message from another node 
	{receive_message, From, To, Message, Message_number} ->
    Coordinator ! {message_done, From, To, Message, Message_number},
  	loop(Coordinator, Name, Class)
  end.

