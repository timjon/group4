-module(diagramcoordinator).
-export([init/4]).

%%Author: Tim Jonasson
%%Collaborators: Isabelle Törnqvist 2017-10-30, Sebastian Fransson 2017-11-06, Pontus Laestadius 2017-12-01
%%Version: 1.9

%Returns no_classes when there was no classes in the given diagram 
init(_Coordinator, _Did, {[], _}, _) -> no_classes;
%Returns no_messages when there was no messages in the given diagram 
init(_Coordinator, _Did, {_, []}, _) -> no_messages;
%Spawns and Initializes the diagram coordinator
init(Coordinator, Did, {Classes, Messages}, Class_names) -> 
	%Sending information that the Coordinator has been spawned. To be printed client-side.
	io:format("no1"),
	Coordinator ! {Did, print_information, ["Spawned diagram coordinator"]},
	io:format("no2"),
	Pids = spawn_nodes(Classes, Did, Coordinator),
	io:format("YEEEEES"),
	loop(Coordinator, Did, Pids, Messages, 1, [], none, Class_names, none). 


%Sends and receives messages until the list of messages is empty  
loop(Coordinator, Did, Pids, [], Message_number, PrevList, ClassDiagram, Class_names, DeploymentDiagram) ->
  receive
    % Links a deployment diagram to the current sequence.
	{deployment_diagram, DeploymentID, Mappings, Coordinator} ->
	  Coordinator ! {deployment_diagram, DeploymentID, Did, Mappings},
	  Coordinator !  {Did, print_information, ["Linked a Deployment Diagram"]},
	  loop(Coordinator, Did, Pids, [], Message_number, PrevList, ClassDiagram, Class_names, {DeploymentID, Mappings});
	  
  % Add a class diagram
  {class_diagram, Classid, Classes, Relations, Coordinator} ->
		Coordinator ! {class_diagram, Classid, Did, {Classes, Relations}},
		%Sends info to the Coordinator that a message has been received by a node. To be printed client-side.
	  Coordinator !  {Did, print_information, ["Linked Class diagram: " ++ name_classes(Pids, Class_names)]},
  	loop(Coordinator, Did, Pids, [], Message_number, PrevList, {Classid, {Classes, Relations}}, Class_names, DeploymentDiagram);
  
  % Sends the class diagram upstreams.
  {request_class_diagram, Pid} ->
  	Pid ! {class_diagram_request, ClassDiagram},
  	loop(Coordinator, Did ,Pids,[],Message_number, PrevList, ClassDiagram, Class_names, DeploymentDiagram);
  
  % Next message
  {next_message, Coordinator} -> 
    Coordinator ! {simulation_done, Did, Message_number},
	  loop(Coordinator, Did ,Pids,[],Message_number, PrevList, ClassDiagram, Class_names, DeploymentDiagram);
	  
	 % Previous message
	{previous_message, Coordinator} ->
	  [Prev_H|Prev_T] = PrevList,
	  Coordinator ! {previous_confirmation, Did, ["Previous message"]},
	  loop(Coordinator, Did, Pids, [Prev_H|[]], Message_number - 1, Prev_T, ClassDiagram, Class_names, DeploymentDiagram)
  end;
 
 %When there are no previous messages this case is used.
  loop(Coordinator, Did, Pids, [L|Ls], Message_number, [], ClassDiagram, Class_names, DeploymentDiagram) ->
    receive
    % Links a deployment diagram to the current sequence.
	{deployment_diagram, DeploymentID, Mappings, Coordinator} ->
	  Coordinator ! {deployment_diagram, DeploymentID, Did, Mappings},
	  Coordinator !  {Did, print_information, ["Linked a Deployment Diagram"]},
	  loop(Coordinator, Did, Pids, [L|Ls], Message_number, [], ClassDiagram, Class_names, {DeploymentID, Mappings});
	
    % Add a class diagram
  {class_diagram, Classid, Classes, Relations, Coordinator} ->
		Coordinator ! {class_diagram, Classid, Did, {Classes, Relations}},
		%Sends info to the Coordinator that a message has been received by a node. To be printed client-side.
	  Coordinator !  {Did, print_information, ["Linked Class diagram: " ++ name_classes(Pids, Class_names)]},
  	loop(Coordinator, Did, Pids, [L|Ls], Message_number, [], {Classid, {Classes, Relations}}, Class_names, DeploymentDiagram);
    
    % Sends the class diagram upstreams.
  {request_class_diagram, Pid} ->
  	Pid ! {class_diagram_request, ClassDiagram},
	  loop(Coordinator, Did, Pids, [L|Ls], Message_number, [], ClassDiagram, Class_names, DeploymentDiagram);
    
    {next_message, Coordinator} -> 
      {From, To, Message} = L,
      
	  	send_message(find_pid(Pids, From), From, To, Message, find_pid(Pids, To), Message_number, Coordinator, Did, get_class_diagram_id(ClassDiagram)), 
      	receive
      
					{message_done, From, To, Message, Message_number} ->
					Coordinator ! {message_sent, Did, From, To, Message, Message_number},
					%Sends info to the Coordinator that a message has been received by a node. To be printed client-side.
					Coordinator !  {Did, print_information, ["Node " ++ atom_to_list(To) ++ " received a message from " ++ atom_to_list(From)]}
	  end,
	  
	  loop(Coordinator, Did, Pids, Ls, Message_number + 1, [L|[]], ClassDiagram, Class_names, DeploymentDiagram);
	  
	  
	{previous_message, Coordinator} ->
	  Coordinator ! {Did, print_information, ["No previous message"]},
	  loop(Coordinator, Did, Pids, [L|Ls], Message_number, [], ClassDiagram, Class_names, DeploymentDiagram)
  end;
  
%This loop runs until the list is empty (when there are no more messages)
loop(Coordinator, Did, Pids, [L|Ls], Message_number, PrevList, ClassDiagram, Class_names, DeploymentDiagram) -> 
  receive
    % Links a deployment diagram to the current sequence.
	{deployment_diagram, DeploymentID, Mappings, Coordinator} ->
	  Coordinator ! {deployment_diagram, DeploymentID, Did, Mappings},
	  Coordinator !  {Did, print_information, ["Linked a Deployment Diagram"]},
	  loop(Coordinator, Did, Pids, [L|Ls], Message_number, PrevList, ClassDiagram, Class_names, {DeploymentID, Mappings});
	  
	% Add a class diagram
  {class_diagram, Classid, Classes, Relations, Coordinator} ->
		Coordinator ! {class_diagram, Classid, Did, {Classes, Relations}},
		%Sends info to the Coordinator that a message has been received by a node. To be printed client-side.
	  Coordinator !  {Did, print_information, ["Linked Class diagram: " ++ name_classes(Pids, Class_names)]},
  	loop(Coordinator, Did, Pids, [L|Ls], Message_number, PrevList, {Classid, {Classes, Relations}}, Class_names, DeploymentDiagram);
  	
  	
  % Sends the class diagram upstreams.
  {request_class_diagram, Pid} ->
  	Pid ! {class_diagram_request, ClassDiagram},
	  loop(Coordinator, Did, Pids, [L|Ls], Message_number, PrevList, ClassDiagram, Class_names, DeploymentDiagram);
  	
  {next_message, Coordinator} -> 
      {From, To, Message} = L,
	  send_message(find_pid(Pids, From), From, To, Message, find_pid(Pids, To), Message_number, Coordinator, Did, get_class_diagram_id(ClassDiagram)), 
      receive
	    	{message_done, From, To, Message, Message_number} ->
		  	Coordinator ! {message_sent, Did, From, To, Message, Message_number},
		  	%Sends info to the Coordinator that a message has been received by a node. To be printed client-side.
		  	Coordinator !  {Did, print_information, ["Node " ++ atom_to_list(To) ++ " received a message from " ++ atom_to_list(From)]}
	  end,
	  loop(Coordinator, Did, Pids, Ls, Message_number + 1, [L|PrevList], ClassDiagram, Class_names, DeploymentDiagram);
	  
	  
  {previous_message, Coordinator} ->
	  [Prev_H| Prev_T] = PrevList,
	  List = [L|Ls],
	  Coordinator ! {previous_confirmation, Did, ["Previous message"]},
	  loop(Coordinator, Did, Pids, [Prev_H| List], Message_number - 1, Prev_T, ClassDiagram, Class_names, DeploymentDiagram)
  end.

  
%checks if a class has a process, none if it does not exist.
find_pid([], _) -> none;
find_pid([{Pid, Class_name}|_], Class_name) -> Pid;
find_pid([_|Ls], Name)                      -> find_pid(Ls, Name). 

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
name_classes(Pids, Names) -> 
	% Returns if all of the names were matched or any errors.
	Result = [iter_split(Pid, Names) || Pid <- Pids],
	atom_to_list(if_err(Result)).
	

% Return if the list has the atom 'err' in it or not.
if_err([]) -> ok;
if_err([err]) -> err;
if_err([err | _]) -> err;
if_err([_ | Rest]) -> if_err(Rest).


% Iterates over the names and splits them to from Class:Name to {Class, [Name]}.  Then matches the Pid with the names.
iter_split({Pid, _}, Names) -> 
	% Call the matching function.
	iter_match(Pid, 
		[{
			% Gets the class.
			hd(string:split(Name, atom_to_list(':'))), 
			% Gets the Name.
			tl(string:split(Name, atom_to_list(':')))} 
			|| Name <- Names]).


% Matches a node's name with a class.
iter_match(_, []) -> ok;

iter_match(Pid, [{Class, [Name]}]) -> 
	case node:getName(Pid) == list_to_atom(Name) of
		true  -> Pid ! {setClass, Class};
		% If the last case is not a match, Return err.
		false -> err
	end;
	
iter_match(Pid, [{Class, [Name]} | Rest]) -> 
	case node:getName(Pid) == list_to_atom(Name) of
		true  -> Pid ! {setClass, Class};
		false -> iter_match(Pid, Rest)
	end.


% Notifies the client if there is a class diagram, to highlight a specific class.
notify_class_diagram(Coordinator, Did, ClassDiagramId, Pid) -> 
	  % If there is a class diagram 
  case ClassDiagramId of 
  % There is no class diagram.
   none -> none;
   % Catches all.	
   _ -> 
		% Gets the name of the class with it's Pid.
		Coordinator ! {class_diagram, ClassDiagramId, Did, highlight, node:getClass(Pid)}
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
