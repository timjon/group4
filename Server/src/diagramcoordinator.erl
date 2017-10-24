-module(diagramcoordinator).
-export([init/1]).

%%Author: Tim Jonasson
%%Version: 1

%Initializes the diagram coordinator by running the function that spawns the nodes and then it starts the loop
init({[], _}) -> no_classes;
init({_, []}) -> no_messages;
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
  Receiver ! {send_message, self(), From, To, Message, To_pid},
  receive
    {send_reply} -> 
	  send_reply_received
  end.