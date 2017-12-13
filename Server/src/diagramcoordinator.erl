-module(diagramcoordinator).
-export([init/3]).

%%Author: Tim Jonasson
%%Collaborators: Isabelle Törnqvist 2017-10-30, Sebastian Fransson 2017-11-06
%%Version: 1.7

%Returns no_classes when there was no classes in the given diagram 
init(_Coordinator, _Did, {[], _}) -> no_classes;
%Returns no_messages when there was no messages in the given diagram 
init(_Coordinator, _Did, {_, []}) -> no_messages;
%Spawns and Initializes the diagram coordinator
init(Coordinator, Did, {L, Messages}) -> 
    {ok, Socket} = gen_tcp:connect("192.168.0.24", 8041, [binary, {packet, 0}]),
	%Sending information that the Coordinator has been spawned. To be printed in the executionlog
	Coordinator ! {Did, print_information, ["Diagram coordinator was spawned"]},
	Pids = spawn_nodes(Socket, L, Did, Coordinator),
	loop(Socket, Coordinator, Did, Pids, Messages, 1, []). 

%This loop runs until the list is empty (when there are no more messages)
loop(Socket, Coordinator, Did, Pids, NextList, Message_number, PrevList) -> 
  receive
    {next_message, Coordinator} -> 
	  case NextList of
	    [] -> 
		  Coordinator ! {simulation_done, Did, Message_number},
	      loop(Socket, Coordinator, Did , Pids, [], Message_number, PrevList);
		
		[L|Ls] -> 
          {From, To, Message} = L,
	      send_message(Socket, From, To, Message, Message_number, Coordinator, Did), 
          receive
		    {tcp, _Socket, Info} ->
			  case binary_to_term(Info) of
	            {message_done, From, To, Message, Message_number} ->
		          Coordinator ! {message_sent, Did, From, To, Message, Message_number},
		          %Sends info to the Coordinator that a message has been received by a node. To be printed in the executionlog
		          Coordinator !  {Did, print_information, ["Node " ++ atom_to_list(To) ++ " received a message from " ++ atom_to_list(From)]}
			  end
	      end,
	      loop(Socket, Coordinator, Did, Pids, Ls, Message_number + 1, [L|PrevList])
	  end;
	  
	{previous_message, Coordinator} ->
	  case PrevList of
	    [] -> 
		  Coordinator ! {Did, print_information, ["No previous message"]},
	      loop(Socket, Coordinator, Did, Pids, NextList, Message_number, []);
		  
	    [Prev_H| Prev_T] -> 
	      Coordinator ! {previous_confirmation, Did, ["Previous message"]},
	      loop(Socket, Coordinator, Did, Pids, [Prev_H| NextList], Message_number - 1, Prev_T)
	  end
  end.

%spawns nodes for every class in the list
spawn_nodes(Socket, List, Did, Coordinator) ->[spawn_node(Socket, Class, Did, Coordinator) || Class <- List].

%spawns a node and returns a tuple with the pid and the class name
spawn_node(Socket, Class_name, Did, Coordinator) -> 
  gen_tcp:send(Socket, term_to_binary({spawn_node, Did, Class_name})),
  receive
    {tcp, _Socket, Info} -> 
      Coordinator ! binary_to_term(Info)
  end.

%sends a message to the given node
send_message(Socket, From, To, Message, Message_number, Coordinator, Did) ->
  gen_tcp:send(Socket, term_to_binary({send_message, From, To, Message, Message_number})),
  message_response(Did, Coordinator).
 
message_response(Did, Coordinator) ->
  receive
    {tcp, Socket, Info} ->
	  case binary_to_term(Info) of
        {send_reply, From, To, Message, Message_number} -> 
		  gen_tcp:send(Socket, term_to_binary({receive_message, From, To, Message, Message_number})),
	      %Sends info to the Coordinator, that the node sucessfully sent a message. To be printed in the executionlog	
	      Coordinator ! {Did, print_information, ["Node " ++ atom_to_list(From) ++ " sent a message to " ++ atom_to_list(To)]}
      end
  end.
  
  
  
  
