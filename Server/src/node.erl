-module(node).
-export([init/1]).

%%Author: Tim Jonasson
%%Collaborators: Pontus Laestadius 2017-12-04
%%Version: 1.4

%Initialized the process for the node
init(Coordinator) ->
  spawn(fun () ->  loop(Coordinator, []) end).

loop(Coordinator, Data) ->
  receive
  
  % Gets the name of the node.
  {Pid, getName} -> 
  	Pid ! {getName, get_last(Data)},
  	loop(Coordinator, Data);
  	
  % Adds data to the node.
  {set, NewData} ->
  	io:format("~p > ~p~n", self(), NewData),
  	loop(Coordinator, [NewData|Data]);
  
    %Case for when the node sends a message for another node
	{send_message, From, To, Message, To_pid, Message_number} ->
	  To_pid ! {receive_message, From, To, Message, Message_number},
	  %Line bellow changed from "Coordinator ! {send_reply},"
	  Coordinator ! {send_reply, From, To, Message, To_pid, Message_number},
      loop(Coordinator, Data);
    
	%Case for when the node receives a message from another node 
	{receive_message, From, To, Message, Message_number} ->
      Coordinator ! {message_done, From, To, Message, Message_number},
      loop(Coordinator, Data)
  end.
  
% Return the last element in a list.
get_last([]) -> [];
get_last([H]) -> H;
get_last([_|T]) -> get_last(T).
