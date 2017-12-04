-module(diagramcoordinator).
-export([init/3]).

%%Author: Tim Jonasson
%%Collaborators: Isabelle Törnqvist 2017-10-30, Sebastian Fransson 2017-11-06, Pontus Laestadius 2017-12-01
%%Version: 1.8

%Returns no_classes when there was no classes in the given diagram 
init(_Coordinator, _Did, {[], _}) -> no_classes;
%Returns no_messages when there was no messages in the given diagram 
init(_Coordinator, _Did, {_, []}) -> no_messages;
%Spawns and Initializes the diagram coordinator
init(Coordinator, Did, {L, Messages}) -> 
	%Sending information that the Coordinator has been spawned. To be printed client-side.
	Coordinator ! {Did, print_information, ["Diagram coordinator was spawned"]},
	Pids = spawn_nodes(L, Did, Coordinator),
	loop(Coordinator, Did, Pids, Messages, 1, [], none). 


%Sends and receives messages until the list of messages is empty  
loop(Coordinator, Did, Pids, [], Message_number, PrevList, ClassDiagram) ->
  receive
  
  % Add a class diagram
  {class_diagram, Classid, Classes, Relations, Coordinator} ->
		Coordinator ! {class_diagram, Classid, Did, {Classes, Relations}},
	  Coordinator !  {Did, print_information, ["Linked Class diagram: " ++ atom_to_list(name_classes(Pids, Classes))]},
  	loop(Coordinator, Did, Pids, [], Message_number, PrevList, {Classid, {Classes, Relations}});
  
  
  {next_message, Coordinator} -> 
    Coordinator ! {simulation_done, Did, Message_number},
	  loop(Coordinator, Did ,Pids,[],Message_number, PrevList, ClassDiagram);
	  
	  
	{previous_message, Coordinator} ->
	  [Prev_H|Prev_T] = PrevList,
	  Coordinator ! {previous_confirmation, Did, ["Previous message"]},
	  loop(Coordinator, Did, Pids, [Prev_H|[]], Message_number - 1, Prev_T, ClassDiagram)
  end;
 
 %When there are no previous messages this case is used.
  loop(Coordinator, Did, Pids, [L|Ls], Message_number, [], ClassDiagram) ->
    receive
    
    % Add a class diagram
  {class_diagram, Classid, Classes, Relations, Coordinator} ->
		Coordinator ! {class_diagram, Classid, Did, {Classes, Relations}},
	  Coordinator !  {Did, print_information, ["Linked Class diagram: " ++ atom_to_list(name_classes(Pids, Classes))]},
  	loop(Coordinator, Did, Pids, [], Message_number, [], {Classid, {Classes, Relations}});
    
    
    {next_message, Coordinator} -> 
      {From, To, Message} = L,
      
	  send_message(find_pid(Pids, From), From, To, Message, find_pid(Pids, To), Message_number, Coordinator, Did, get_class_diagram_id(ClassDiagram)), 
      receive
      
	    {message_done, From, To, Message, Message_number} ->
		  Coordinator ! {message_sent, Did, From, To, Message, Message_number},
		  %Sends info to the Coordinator that a message has been received by a node. To be printed client-side.
		  Coordinator !  {Did, print_information, ["Node " ++ atom_to_list(To) ++ " received a message from " ++ atom_to_list(From)]}
	  end,
	  
	  loop(Coordinator, Did, Pids, Ls, Message_number + 1, [L|[]], ClassDiagram);
	  
	  
	{previous_message, Coordinator} ->
	  Coordinator ! {Did, print_information, ["No previous message"]},
	  loop(Coordinator, Did, Pids, [L|Ls], Message_number, [], ClassDiagram)
  end;
  
%This loop runs until the list is empty (when there are no more messages)
loop(Coordinator, Did, Pids, [L|Ls], Message_number, PrevList, ClassDiagram) -> 
  receive

	% Add a class diagram
  {class_diagram, Classid, Classes, Relations, Coordinator} ->
		Coordinator ! {class_diagram, Classid, Did, {Classes, Relations}},
	  Coordinator !  {Did, print_information, ["Linked Class diagram: " ++ atom_to_list(name_classes(Pids, Classes))]},
  	loop(Coordinator, Did, Pids, [], Message_number, PrevList, {Classid, {Classes, Relations}});
  	
  	
    {next_message, Coordinator} -> 
      {From, To, Message} = L,
	  send_message(find_pid(Pids, From), From, To, Message, find_pid(Pids, To), Message_number, Coordinator, Did, get_class_diagram_id(ClassDiagram)), 
      receive
	    {message_done, From, To, Message, Message_number} ->
		  Coordinator ! {message_sent, Did, From, To, Message, Message_number},
		  %Sends info to the Coordinator that a message has been received by a node. To be printed client-side.
		  Coordinator !  {Did, print_information, ["Node " ++ atom_to_list(To) ++ " received a message from " ++ atom_to_list(From)]}
	  end,
	  loop(Coordinator, Did, Pids, Ls, Message_number + 1, [L|PrevList], ClassDiagram);
	  
	  
	{previous_message, Coordinator} ->
	  [Prev_H| Prev_T] = PrevList,
	  List = [L|Ls],
	  Coordinator ! {previous_confirmation, Did, ["Previous message"]},
	  loop(Coordinator, Did, Pids, [Prev_H| List], Message_number - 1, Prev_T, ClassDiagram)
  end.

  
%checks if a class has a process, none if it does not exist.
find_pid([], _) -> none;
find_pid([{Pid, Class_name}|_], Class_name) -> Pid;
find_pid([_|Ls], Name)                      -> find_pid(Ls, Name). 


%spawns nodes for every class in the list
spawn_nodes(List, Did, Coordinator) ->[spawn_node(Class, Did, Coordinator) || Class <- List].



% Gets the name of a specific node.
getName(NodePid) ->
	NodePid ! {self(), getName},
	receive 
		{getName, Name} -> Name
		
	after
		1000 ->
		  '404'
	end.


% Returns the wrapped Class diagram id or none.
get_class_diagram_id(none) -> none;
get_class_diagram_id({Id, {_, _}}) -> Id.


%spawns a node and returns a tuple with the pid and the class name
spawn_node(Class_name, Did, Coordinator) -> 
  Self = self(),
  %Sends info to the Coordinator, that a node has been spawned. To be printed in the executionlog
  Coordinator ! {Did, print_information, ["Spawned node " ++ atom_to_list(Class_name)]},
  {node:init(Self), Class_name}.



%% Sets the nodes class names and fields. 

% No more classes, Valid case because no more processes.
name_classes([], [[]]) -> ok;
% No more processes. Invalid case when you run out of pids but still got classes.
name_classes([], _) -> err;
% No more classes. Invalid case when you run out of classes but still got pids.
name_classes(_, [[]]) -> err;
% Only has a class and no fields.
name_classes([Pid | PidRest], [[Class] | Classes]) -> 
	Pid ! {setName, Class},
	name_classes(PidRest, [Classes]);
% Has class and fields.
name_classes([Pid | PidRest], [[Class | Fields] | Classes]) -> % ------------------------------------------------------------- BROKEN RIGHT HERE
	Pid ! {setFields, Fields},
	Pid ! {setName, Class},
	name_classes(PidRest, [Classes]).


%sends a message to the given node
send_message(Receiver, From, To, Message, To_pid, Message_number, Coordinator, Did, ClassId) ->

  % If there is a class diagram 
  case ClassId of 
   none -> none;
   % Catches all.
   _ -> 

		% Gets the name of the class with it's Pid.
		Name = getName(To_pid),
	
		% Matches if it has a name or not.
		case Name of
	
			% None has been provided.
			none  -> none;
		
			% Could not talk to the node.
			'404' -> err;
		
			% Catches all valid names.
			% Tells the client to highlight it.
			Valid -> Coordinator ! {class_diagram, ClassId, Did, higlight, Valid}
		end
	end,

	
  Receiver ! {send_message, From, To, Message, To_pid, Message_number},
  receive
    {send_reply, From, To, Message, To_pid, Message_number} -> 
		%Sends info to the Coordinator, that the node sucessfully sent a message. To be printed in the executionlog	
		Coordinator ! {Did, print_information, ["Node " ++ atom_to_list(From) ++ " sent a message to " ++ atom_to_list(To)]} 
		end.
