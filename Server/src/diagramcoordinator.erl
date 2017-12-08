-module(diagramcoordinator).
-export([init/4]).

%%Author: Tim Jonasson
%%Collaborators: Isabelle Törnqvist 2017-10-30, Sebastian Fransson 2017-11-06, Pontus Laestadius 2017-12-01
%%Version: 1.8

%Returns no_classes when there was no classes in the given diagram 
init(_Coordinator, _Did, {[], _}, _) -> no_classes;
%Returns no_messages when there was no messages in the given diagram 
init(_Coordinator, _Did, {_, []}, _) -> no_messages;
%Spawns and Initializes the diagram coordinator
init(Coordinator, Did, {Classes, Messages}, Class_names) -> 
	%Sending information that the Coordinator has been spawned. To be printed client-side.
	Coordinator ! {Did, print_information, ["Spawned diagram coordinator"]},
	Pids = spawn_nodes(Classes, Did, Coordinator),
	loop(Coordinator, Did, Pids, Messages, 1, [], none, Class_names). 


%Sends and receives messages until the list of messages is empty  
loop(Coordinator, Did, Pids, [], Message_number, PrevList, ClassDiagram, Class_names) ->
  receive
  
  % Add a class diagram
  {class_diagram, Classid, Classes, Relations, Coordinator} ->
		Coordinator ! {class_diagram, Classid, Did, {Classes, Relations}},
		%Sends info to the Coordinator that a message has been received by a node. To be printed client-side.
	  Coordinator !  {Did, print_information, ["Linked Class diagram: " ++ atom_to_list(name_classes(Pids, Class_names))]},
	 
  	loop(Coordinator, Did, Pids, [], Message_number, PrevList, {Classid, {Classes, Relations}}, Class_names);
  
  % Sends the class diagram upstreams.
  {request_class_diagram, Pid} ->
  	Pid ! {class_diagram_request, ClassDiagram},
  	loop(Coordinator, Did ,Pids,[],Message_number, PrevList, ClassDiagram, Class_names);
  
  % Next message
  {next_message, Coordinator} -> 
    Coordinator ! {simulation_done, Did, Message_number},
	  loop(Coordinator, Did ,Pids,[],Message_number, PrevList, ClassDiagram, Class_names);
	  
	 % Previous message
	{previous_message, Coordinator} ->
	  [Prev_H|Prev_T] = PrevList,
	  Coordinator ! {previous_confirmation, Did, ["Previous message"]},
	  loop(Coordinator, Did, Pids, [Prev_H|[]], Message_number - 1, Prev_T, ClassDiagram, Class_names)
  end;
 
 %When there are no previous messages this case is used.
  loop(Coordinator, Did, Pids, [L|Ls], Message_number, [], ClassDiagram, Class_names) ->
    receive
    
    % Add a class diagram
  {class_diagram, Classid, Classes, Relations, Coordinator} ->
		Coordinator ! {class_diagram, Classid, Did, {Classes, Relations}},
		%Sends info to the Coordinator that a message has been received by a node. To be printed client-side.
	  Coordinator !  {Did, print_information, ["Linked Class diagram: " ++ atom_to_list(name_classes(Pids, Class_names))]},
  	loop(Coordinator, Did, Pids, [L|Ls], Message_number, [], {Classid, {Classes, Relations}}, Class_names);
    
    % Sends the class diagram upstreams.
  {request_class_diagram, Pid} ->
  	Pid ! {class_diagram_request, ClassDiagram},
	  loop(Coordinator, Did, Pids, [L|Ls], Message_number, [], ClassDiagram, Class_names);
    
    {next_message, Coordinator} -> 
      {From, To, Message} = L,
      
	  	send_message(find_pid(Pids, From), From, To, Message, find_pid(Pids, To), Message_number, Coordinator, Did, get_class_diagram_id(ClassDiagram)), 
      	receive
      
					{message_done, From, To, Message, Message_number} ->
					Coordinator ! {message_sent, Did, From, To, Message, Message_number},
					%Sends info to the Coordinator that a message has been received by a node. To be printed client-side.
					Coordinator !  {Did, print_information, ["Node " ++ atom_to_list(To) ++ " received a message from " ++ atom_to_list(From)]}
	  end,
	  
	  loop(Coordinator, Did, Pids, Ls, Message_number + 1, [L|[]], ClassDiagram, Class_names);
	  
	  
	{previous_message, Coordinator} ->
	  Coordinator ! {Did, print_information, ["No previous message"]},
	  loop(Coordinator, Did, Pids, [L|Ls], Message_number, [], ClassDiagram, Class_names)
  end;
  
%This loop runs until the list is empty (when there are no more messages)
loop(Coordinator, Did, Pids, [L|Ls], Message_number, PrevList, ClassDiagram, Class_names) -> 
  receive

	% Add a class diagram
  {class_diagram, Classid, Classes, Relations, Coordinator} ->
		Coordinator ! {class_diagram, Classid, Did, {Classes, Relations}},
		%Sends info to the Coordinator that a message has been received by a node. To be printed client-side.
	  Coordinator !  {Did, print_information, ["Linked Class diagram: " ++ atom_to_list(name_classes(Pids, Class_names))]},
  	loop(Coordinator, Did, Pids, [L|Ls], Message_number, PrevList, {Classid, {Classes, Relations}}, Class_names);
  	
  	
  % Sends the class diagram upstreams.
  {request_class_diagram, Pid} ->
  	Pid ! {class_diagram_request, ClassDiagram},
	  loop(Coordinator, Did, Pids, [L|Ls], Message_number, PrevList, ClassDiagram, Class_names);
  	
    {next_message, Coordinator} -> 
      {From, To, Message} = L,
	  send_message(find_pid(Pids, From), From, To, Message, find_pid(Pids, To), Message_number, Coordinator, Did, get_class_diagram_id(ClassDiagram)), 
      receive
	    	{message_done, From, To, Message, Message_number} ->
		  	Coordinator ! {message_sent, Did, From, To, Message, Message_number},
		  	%Sends info to the Coordinator that a message has been received by a node. To be printed client-side.
		  	Coordinator !  {Did, print_information, ["Node " ++ atom_to_list(To) ++ " received a message from " ++ atom_to_list(From)]}
	  end,
	  loop(Coordinator, Did, Pids, Ls, Message_number + 1, [L|PrevList], ClassDiagram, Class_names);
	  
	  
	{previous_message, Coordinator} ->
	  [Prev_H| Prev_T] = PrevList,
	  List = [L|Ls],
	  Coordinator ! {previous_confirmation, Did, ["Previous message"]},
	  loop(Coordinator, Did, Pids, [Prev_H| List], Message_number - 1, Prev_T, ClassDiagram, Class_names)
  end.

  
%checks if a class has a process, none if it does not exist.
find_pid([], _) -> none;
find_pid([{Pid, Class_name}|_], Class_name) -> Pid;
find_pid([_|Ls], Name)                      -> find_pid(Ls, Name). 


% Gets the name of a specific node.
getClass(NodePid) ->
	NodePid ! {self(), getClass},
	receive 
		{getClass, Class} -> Class
		
	after
		1000 ->
		  err
	end.


% Returns the wrapped Class diagram id or none.
get_class_diagram_id(none) -> none;
get_class_diagram_id({Id, {_, _}}) -> Id.


%spawns nodes for every class in the list
spawn_nodes(List, Did, Coordinator) ->[spawn_node(Class, Did, Coordinator) || Class <- List].


%spawns a node and returns a tuple with the pid and the class name
spawn_node(Class_name, Did, Coordinator) -> 
  Self = self(),
  %Sends info to the Coordinator, that a node has been spawned. To be printed in the executionlog
  Coordinator ! {Did, print_information, ["Spawned node " ++ atom_to_list(Class_name)]},
  {node:init(Self, Class_name), Class_name}.


% Iterates over the Pids and classes and propogates the data to the Pid.
name_classes(Pids, Names) -> [find_a_thing(Pid, Names) || Pid <- Pids], ok.

find_a_thing(Pid, Names) -> 
	find_a_thing2(Pid, 
		[{
			hd(string:split(Name, atom_to_list(':'))), 
			tl(string:split(Name, atom_to_list(':')))} 
			|| Name <- Names]).

find_a_thing2(_, []) -> ok;
find_a_thing2({Pid, Name}, [{Class, [Name]} | _]) -> 
	Pid ! {setClass, Class};
find_a_thing2(Pid, [_ | Rest]) -> find_a_thing2(Pid, Rest).
	
	
% Notifies the client if there is a class diagram, to highlight a specific class.
notify_class_diagram(Coordinator, Did, ClassDiagramId, Pid) -> 
	  % If there is a class diagram 
  case ClassDiagramId of 
  % There is no class diagram.
   none -> none;
   % Catches all.	
   _ -> 
		% Gets the name of the class with it's Pid.
		Name = getClass(Pid),
		% Matches if it has a name or not.
		case Name of
			% None has been provided.
			none  -> none;
			% Node did not responde.
			err -> err;
			% Catches all valid names.
			% Propogates to highlight the class.
			Valid -> Coordinator ! {class_diagram, ClassDiagramId, Did, highlight, Valid}
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
