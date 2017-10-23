-module(diagramcoordinator).
-export([find_pid/2, spawn_nodes/1, spawn_node/1, kill_message/1, init/2]).
%-import(node, [init/1]).

%%Author: Tim Jonasson



%Initializes the diagram coordinator by running the function that spawns the nodes and then it starts the loop
init(_, {[], _}) -> no_classes;
init(_, {L, []}) -> no_messages;
init(Parent_pid, {L, Messages}) -> 
  io:format("hello1, ~p~n", [Parent_pid]),
  Pids = spawn_nodes(L),
  loop(Parent_pid, Pids, Messages, 1).
  
  
%Sends and receives messages until the list of messages is empty  
loop(Parent_pid, Pids, [], Message_number) ->
  receive
    {next_message, Pid} -> 
	  Pid ! ok,
      io:format("Simulation finished ~n "),
      io:format("hello3, ~p~n", [Parent_pid]),
      Pid ! {simulation_done, Message_number},
      io:format("hello4, ~p~n", [Parent_pid]),
      io:format("bye~n")
	end;
loop(Parent_pid, Pids, [L|Ls], Message_number) -> 
  receive
    {next_message, Pid} -> 
	  io:format("hello2 ~p ~n", [Pid]),
	  Pid ! ok,
      {From, To, Message} = L,
	  send_message(find_pid(Pids, From), From, To, Message, find_pid(Pids, To)),
      io:format("~p", [From]),
      io:format(" sent a message to ~p", [To]),
      io:format("~n"),
      receive
	    {message_done, From, To, Message} ->
		  Parent_pid ! {message_sent, From, To, Message, Message_number}
	  end,
	  loop(Parent_pid, Pids, Ls, Message_number + 1)
  end.
  


%checks if a class has a process and spawns it if it doesnt exist
%find_pid([], Name)                          -> spawn_node(Name);
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