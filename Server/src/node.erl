-module(node).
-export([init/2]).

%%Author: Tim Jonasson
%%Collaborators: Pontus Laestadius 2017-12-04
%%Version: 2.0

%Initialized the process for the node
init(Coordinator, Name) ->
  spawn(fun () ->  loop(Coordinator, Name, none) end).

loop(Coordinator, Name, Class) ->
  receive
  	% Gets the class of the node.
    {Pid, getClass} -> 
  	  Pid ! {getClass, Class},
  	  loop(Coordinator, Name, Class);
  	
    % Sets the class of the node.
    {setClass, NewClass} ->
  	  loop(Coordinator, Name, NewClass);
  
    %Case for when the node sends a message for another node
    {send_message, From, To, Message, Message_number} ->
	  Coordinator ! {send_reply, From, To, Message, Message_number},
      loop(Coordinator, Name, Class);
    
	%Case for when the node receives a message from another node 
	{receive_message, From, To, Message, Message_number} ->
      Coordinator ! {message_done, From, To, Message, Message_number},
  	  loop(Coordinator, Name, Class)
  end.
