-module(diagramcoordinator).
-export([find_pid/2, spawn_nodes/1, spawn_node/1, kill_message/1, send_message/1, init/2]).
-import(node, [init/1]).

%%Author: Tim Jonasson



%Initializes the diagram coordinator by running the function that spawns the nodes and then it starts the loop
init([], _) -> no_classes;
init(L, []) -> {no_messages, L};
init(L, Messages) -> 
  Pids = spawn_nodes(L),
  loop(Pids, Messages).
  
  
%Sends and receives messages until the list of messages is empty  
loop(Pids, []) -> simulation_finished;
loop(Pids, [L|Ls]) -> 
  {From, To, Message} = L,
  send_message(find_pid(Pids, From)),
  io:format("~p", [From]),
  io:format(" sent a message to ~p", [To]),
  io:format("~n"),
  receive_message(find_pid(Pids, To)),
  io:format("~p", [To]),
  io:format(" received a message from ~p", [From]),
  io:format("~n"),
  loop(Pids, Ls).


%checks if a class has a process and spawns it if it doesnt exist
find_pid([], Name)                          -> spawn_node(Name);
find_pid([{Pid, Class_name}|_], Class_name) -> Pid;
find_pid([_|Ls], Name)                      -> find_pid(Ls, Name). 

%spawns nodes for every class in the list
spawn_nodes(List) ->[spawn_node(Class) || Class <- List].

%spawns a node and returns a tuple with the pid and the class name
spawn_node(Class_name) -> 
  {node:init(self()), Class_name}.

%sends a message to the given node
send_message(Receiver) ->
  Receiver ! {send_message},
  receive
    {send_reply} -> 
	  send_reply_received
  end.
  
%Receives a message to the given node
receive_message(Receiver) ->
  Receiver ! {receive_message},
  receive
    {receive_reply} -> 
	  receive_reply_received
  end.
  
%Kills the given node the given node
kill_message(Receiver) ->  
  Receiver ! {kill_message},
  receive
    {kill_reply} -> 
	  kill_reply_received
  end.