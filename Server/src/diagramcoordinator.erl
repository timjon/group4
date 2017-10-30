-module(diagramcoordinator).
-export([init/3]).

%%Author: Tim Jonasson
%%Version: 1.3

%Returns no_classes when there was no classes in the given diagram 
init(_Usercoordinator, _Did, {[], _}) -> no_classes;
%Returns no_messages when there was no messages in the given diagram 
init(_Usercoordinator, _Did, {_, []}) -> no_messages;
%Spawns and Initializes the diagram coordinator
init(Usercoordinator, Did, {L, Messages}) -> 
  Pids = spawn_nodes(L),
  loop(Usercoordinator, Did, Pids, Messages, 1).
  
%Sends and receives messages until the list of messages is empty  
loop(Usercoordinator, Did, _, [], Message_number) ->
  receive
    {next_message, Usercoordinator} -> 
	  Usercoordinator ! ok,
      Usercoordinator ! {simulation_done, Did, Message_number}
  end;
%This loop runs until the list is empty (when there are no more messages)
loop(Usercoordinator, Did, Pids, [L|Ls], Message_number) -> 
  receive
    {next_message, Usercoordinator} -> 
	  Usercoordinator ! ok,
      {From, To, Message} = L,

	  send_message(find_pid(Pids, From), From, To, Message, find_pid(Pids, To), Message_number),
	  
      receive
	    {message_done, From, To, Message, Message_number} ->
		  Usercoordinator ! {message_sent, Did, From, To, Message, Message_number}
	  end,
	  loop(Usercoordinator, Did, Pids, Ls, Message_number + 1)
  end.
  
%checks if a class has a process and spawns it if it doesnt exist
find_pid([{Pid, Class_name}|_], Class_name) -> Pid;
find_pid([_|Ls], Name)                      -> find_pid(Ls, Name). 

%spawns nodes for every class in the list
spawn_nodes(List) ->[spawn_node(Class) || Class <- List].

%spawns a node and returns a tuple with the pid and the class name
spawn_node(Class_name) -> 
  Self = self(),
  {node:init(Self), Class_name}.

%sends a message to the given node
send_message(Receiver, From, To, Message, To_pid, Message_number) ->
  Receiver ! {send_message, From, To, Message, To_pid, Message_number},

  receive
    {send_reply, From} -> 
	  Pid ! {send_reply_received, From, To},
	  io:format("Some string")
  end.
