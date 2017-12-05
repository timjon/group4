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
		  err
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


% Iterates over the Pids and classes and propogates the data to the Pid.
name_classes(Pids, Classes) -> 
  [
  	[Pid ! {set, Item} || Item <- Class]
  	|| 
  	Class <- Classes, 
  	{Pid, _} <- Pids
  ],
  ok.


% Notifies the client if there is a class diagram, to highlight a specific class.
notify_class_diagram(Coordinator, Did, ClassDiagramId, Pid) -> 
	  % If there is a class diagram 
  case ClassDiagramId of 
  
  % There is no class diagram.
   none -> none;
   % Catches all.
   _ -> 

		% Gets the name of the class with it's Pid.
		Name = getName(Pid),
	
		% Matches if it has a name or not.
		case Name of
	
			% None has been provided.
			none  -> none;
		
			% Node did not responde.
			err -> err;
		
			% Catches all valid names.
			% Tells the client to highlight it.
			Valid -> Coordinator ! {class_diagram, ClassDiagramId, Did, higlight, Valid}
		end
	end.
	

%sends a message to the given node
send_message(Receiver, From, To, Message, To_pid, Message_number, Coordinator, Did, ClassId) ->

	% Notify the class diagram to highlight a specific class.
	notify_class_diagram(Coordinator, Did, ClassId, To_pid),
	
  Receiver ! {send_message, From, To, Message, To_pid, Message_number},
  receive
    {send_reply, From, To, Message, To_pid, Message_number} -> 
		%Sends info to the Coordinator, that the node sucessfully sent a message. To be printed in the executionlog	
		Coordinator ! {Did, print_information, ["Node " ++ atom_to_list(From) ++ " sent a message to " ++ atom_to_list(To)]} 
		end.
