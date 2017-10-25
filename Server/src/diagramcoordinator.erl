-module(diagramcoordinator).
-export([init/1]).

%%Author: Tim Jonasson
%%Version: 1.1

%Returns no_classes when there was no classes in the given diagram 
init({[], _}) -> no_classes;
%Returns no_messages when there was no messages in the given diagram 
init({_, []}) -> no_messages;
%Spawns and Initializes the diagram coordinator
init({L, Messages}) -> 
  Pids = spawn_nodes(L),
  loop(Pids, Messages, 1).
  
%Sends and receives messages until the list of messages is empty  
loop(_, [], Message_number) ->
  receive
    {next_message, Pid} -> 
	  Pid ! ok,
      Pid ! {simulation_done, Message_number}
  end;
%This loop runs until the list is empty (when there are no more messages)
loop(Pids, [L|Ls], Message_number) -> 
  receive
    {next_message, Pid} -> 
	  Pid ! ok,
      {From, To, Message} = L,
	  send_message(find_pid(Pids, From), From, To, Message, find_pid(Pids, To)),
      receive
	    {message_done, From, To, Message} ->
		  Pid ! {message_sent, From, To, Message, Message_number}
	  end,
	  loop(Pids, Ls, Message_number + 1)
  end.
  
%checks if a class has a process and spawns it if it doesnt exist
find_pid([{Pid, Class_name}|_], Class_name) -> Pid;
find_pid([_|Ls], Name)                      -> find_pid(Ls, Name). 

%spawns nodes for every class in the list
spawn_nodes(List) ->[spawn_node(Class) || Class <- List].

%spawns a node and returns a tuple with the pid and the class name
spawn_node(Class_name) -> 
  {node:init(self()), Class_name}.

%sends a message to the given node
send_message(Receiver, From, To, Message, To_pid) ->
  Receiver ! {send_message, From, To, Message, To_pid},
  receive
    {send_reply} -> 
	  send_reply_received
  end.