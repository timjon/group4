-module(diagramcoordinator).
-export([init/3]).

%%Author: Tim Jonasson
%%Collaborator: Isabelle Törnqvist 2017-10-30, Sebastian Fransson 2017-11-08
%%Version: 1.6

%Returns no_classes when there was no classes in the given diagram 
init(_Coordinator, _Did, {[], _}) -> no_classes;
%Returns no_messages when there was no messages in the given diagram 
init(_Coordinator, _Did, {_, []}) -> no_messages;
%Spawns and Initializes the diagram coordinator
init(Coordinator, Did, {L, Messages}) -> 
	%Sending information that the coordinator has been spawned. To be printed in the executionlog
	Coordinator ! {Did, print_information, ["Diagram coordinator was spawned"]},
	Pids = spawn_nodes(L, Did, Coordinator),
	loop(Coordinator, Did, Pids, Messages, 1). 
%Sends and receives messages until the list of messages is empty  
loop(Coordinator, Did, _, [], Message_number) ->
  receive
    {next_message, Coordinator} -> 
	  Coordinator ! ok,
      Coordinator ! {simulation_done, Did, Message_number}
  end;
%This loop runs until the list is empty (when there are no more messages)
loop(Coordinator, Did, Pids, [L|Ls], Message_number) -> 
  receive
    {next_message, Coordinator} -> 
	  Coordinator ! ok,
      {From, To, Message} = L,
	  send_message(find_pid(Pids, From), From, To, Message, find_pid(Pids, To), Message_number, Coordinator, Did), 
      receive
	    {message_done, From, To, Message, Message_number} ->
		  Coordinator ! {message_sent, Did, From, To, Message, Message_number},
		  %Sends info to the usercoordinator that a message has been received by a node. To be printed in the executionlog
		  Coordinator !  {Did, print_information, ["Node " ++ atom_to_list(To) ++ " received a message from " ++ atom_to_list(From)]}
	  end,
	  loop(Coordinator, Did, Pids, Ls, Message_number + 1)
  end.
  
%checks if a class has a process and spawns it if it doesnt exist
find_pid([{Pid, Class_name}|_], Class_name) -> Pid;
find_pid([_|Ls], Name)                      -> find_pid(Ls, Name). 

%spawns nodes for every class in the list
spawn_nodes(List, Did, Coordinator) ->[spawn_node(Class, Did, Coordinator) || Class <- List].

%spawns a node and returns a tuple with the pid and the class name
spawn_node(Class_name, Did, Coordinator) -> 
  Self = self(),
  %Sends info to the usercoordinator, that a node has been spawned. To be printed in the executionlog
  Coordinator ! {Did, print_information, ["Spawned node " ++ atom_to_list(Class_name)]},
  {node:init(Self), Class_name}.

%sends a message to the given node
send_message(Receiver, From, To, Message, To_pid, Message_number, Coordinator, Did) ->
  Receiver ! {send_message, From, To, Message, To_pid, Message_number},
  receive
    {send_reply, From, To, Message, To_pid, Message_number} -> 
		%Sends info to the usercoordinator, that the node sucessfully sent a message. To be printed in the executionlog	
		Coordinator ! {Did, print_information, ["Node " ++ atom_to_list(From) ++ " sent a message to " ++ atom_to_list(To)]} 
		end.