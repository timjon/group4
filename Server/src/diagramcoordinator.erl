-module(diagramcoordinator).
-export([find_pid/2, spawn_nodes/1, spawn_node/1, kill_message/1, send_message/1]).
-import(node, [init/1]).


init([], _) -> no_classes;
init(L, []) -> {no_messages, L};
init(L, Messages) -> 
  Pids = spawn_nodes(L),
  loop(Pids, Messages).

loop(Pids, []) -> simulation_finished;
loop(Pids, [L|Ls]) -> ok.

find_pid([], Name)                          -> spawn_node(Name);
find_pid([{Pid, Class_name}|_], Class_name) -> Pid;
find_pid([_|Ls], Name)                      -> find_pid(Ls, Name). 

spawn_nodes(List) ->[spawn_node(Class) || Class <- List].

spawn_node(Class_name) -> 
  {node:init(self()), Class_name}.

send_message(Receiver) ->
  Receiver ! {send_message},
  receive
    {send_reply} -> 
	  send_reply_received
  end.
  
receive_message(Receiver) ->
  Receiver ! {receive_message},
  receive
    {receive_reply} -> 
	  receive_reply_received
  end.
  
kill_message(Receiver) ->  
  Receiver ! {kill_message},
  receive
    {kill_reply} -> 
	  kill_reply_received
  end.