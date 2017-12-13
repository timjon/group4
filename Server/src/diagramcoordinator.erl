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
init(Coordinator, Did, {Classes, Messages},  Class_names) -> 
  Sockets = tcp_connect("10.0.48.187", [8041, 8042, 8043]),
  %Sending information that the Coordinator has been spawned. To be printed in the executionlog
  io:format("hello"),
  Coordinator ! {Did, print_information, ["Diagram coordinator was spawned"]},
  Nodes = spawn_nodes(Sockets, Classes, Did, Coordinator, []),
  loop(Sockets, Coordinator, Did, Nodes, Messages, 1, [], none, Class_names). 
	
tcp_connect(_Ip, []) -> [];
tcp_connect(Ip,  [Port|Ports]) ->
  {ok, Socket} = gen_tcp:connect(Ip, Port, [binary, {packet, 0}]),
  [Socket | tcp_connect(Ip, Ports)].

%This loop runs until the list is empty (when there are no more messages)
loop(Sockets, Coordinator, Did, Classes, NextList, Message_number, PrevList, ClassDiagram, Class_names) -> 
  receive
	% Add a class diagram
    {class_diagram, Classid, Classes, Relations, Coordinator} ->
    io:format("classdiagram"),
	  Coordinator ! {class_diagram, Classid, Did, {Classes, Relations}},
	  %Sends info to the Coordinator that a message has been received by a node. To be printed client-side.
	  Coordinator !  {Did, print_information, ["Linked Class diagram: " ++ name_classes(Classes, Class_names, Classes)]},
  	  loop(Sockets, Coordinator, Did, Sockets, Classes, Message_number, PrevList, {Classid, {Classes, Relations}}, Class_names);
  	
  	
    % Sends the class diagram upstreams.
    {request_class_diagram, Pid} ->
  	  Pid ! {class_diagram_request, ClassDiagram},
	  loop(Sockets, Coordinator, Did, Sockets, Classes, Message_number, PrevList, ClassDiagram, Class_names);
  	
    {next_message, Coordinator} -> 
	  case NextList of
	    [] -> 
		  Coordinator ! {simulation_done, Did, Message_number},
	      loop(Sockets, Coordinator, Did , Classes, [], Message_number, PrevList, ClassDiagram, Class_names);
		
		[L|Ls] -> 
          {From, To, Message} = L,
		  FromSocket = find_class(Classes, From),
		  ToSocket = find_class(Classes, To),
	      send_message(Classes, FromSocket, ToSocket, From, To, Message, Message_number, Coordinator, Did, get_class_diagram_id(ClassDiagram)), 
          receive
		    {tcp, _Socket, Info} ->
			  case binary_to_term(Info) of
	            {message_done, From, To, Message, Message_number} ->
		          Coordinator ! {message_sent, Did, From, To, Message, Message_number},
		          %Sends info to the Coordinator that a message has been received by a node. To be printed in the executionlog
		          Coordinator !  {Did, print_information, ["Node " ++ atom_to_list(To) ++ " received a message from " ++ atom_to_list(From)]}
			  end
	      end,
	      loop(Sockets, Coordinator, Did, Classes, Ls, Message_number + 1, [L|PrevList], ClassDiagram, Class_names)
	  end;
	  
	{previous_message, Coordinator} ->
	  case PrevList of
	    [] -> 
		  Coordinator ! {Did, print_information, ["No previous message"]},
	      loop(Sockets, Coordinator, Did, Classes, NextList, Message_number, [], ClassDiagram, Class_names);
		  
	    [Prev_H| Prev_T] -> 
	      Coordinator ! {previous_confirmation, Did, ["Previous message"]},
	      loop(Sockets, Coordinator, Did, Classes, [Prev_H| NextList], Message_number - 1, Prev_T, ClassDiagram, Class_names)
	  end
  end.

% Returns the wrapped Class diagram id or none.
get_class_diagram_id(none) -> none;
get_class_diagram_id({Id, {_, _}}) -> Id.


%spawns nodes for every class in the list
spawn_nodes(_, [], _, _, _) -> [];
spawn_nodes([], Classes, Did, Coordinator, UsedSockets) -> spawn_nodes(UsedSockets, Classes, Did, Coordinator, []);
spawn_nodes([Socket|Sockets], [Class|Classes], Did, Coordinator, UsedSockets) ->
  [spawn_node(Socket, Class, Did, Coordinator)| spawn_nodes(Sockets, Classes, Did, Coordinator, [Socket|UsedSockets])].


%spawns a node and returns a tuple with the pid and the class name
spawn_node(Socket, Class_name, Did, Coordinator) -> 
  gen_tcp:send(Socket, term_to_binary({spawn_node, Did, Class_name})),
  receive
    {tcp, _Socket, Info} -> 
      Coordinator ! binary_to_term(Info)
  end,
  {Socket, Class_name}.

%sends a message to the given node
send_message(Classes, FromSocket, ToSocket, From, To, Message, Message_number, Coordinator, Did, ClassDiagramId) ->
  notify_class_diagram(Classes, Coordinator, Did, ClassDiagramId, To),
  gen_tcp:send(FromSocket, term_to_binary({send_message, From, To, Message, Message_number})),
  message_response(ToSocket, Did, Coordinator).
 
message_response(ToSocket, Did, Coordinator) ->
  receive
    {tcp, _Socket, Info} ->
	  case binary_to_term(Info) of
        {send_reply, From, To, Message, Message_number} -> 
		  gen_tcp:send(ToSocket, term_to_binary({receive_message, From, To, Message, Message_number})),
	      %Sends info to the Coordinator, that the node sucessfully sent a message. To be printed in the executionlog	
	      Coordinator ! {Did, print_information, ["Node " ++ atom_to_list(From) ++ " sent a message to " ++ atom_to_list(To)]}
      end
  end.

% Iterates over the Pids and classes and propogates the data to the Pid.
name_classes(Class_names, Names, Classes) -> 
	% Returns if all of the names were matched or any errors.
	Result = [iter_split(Class_name, Names, Classes) || Class_name <- Class_names],
	atom_to_list(if_err(Result)).
	
% Return if the list has the atom 'err' in it or not.
if_err([]) -> ok;
if_err([err]) -> err;
if_err([err | _]) -> err;
if_err([_ | Rest]) -> if_err(Rest).

% Iterates over the names and splits them to from Class:Name to {Class, [Name]}.  Then matches the Pid with the names.
iter_split({Socket, _}, Names, Classes) -> 
	% Call the matching function.
	iter_match(Socket, 
		[{
			% Gets the class.
			hd(string:split(Name, atom_to_list(':'))), 
			% Gets the Name.
			tl(string:split(Name, atom_to_list(':')))} 
			|| Name <- Names], Classes).


% Matches a node's name with a class.
iter_match(_, [], _) -> ok;

iter_match(Socket, [{Class, [Name]}], Classes) -> 
	case getName(Classes, Name) == list_to_atom(Name) of
		true  -> gen_tcp:send(Socket, term_to_binary({setClass, Class, Name}));
		% If the last case is not a match, Return err.
		false -> err
	end;
	
iter_match(Socket, [{Class, [Name]} | Rest], Classes) -> 
	case getName(Classes, Name) == list_to_atom(Name) of
		true  -> gen_tcp:send(Socket, term_to_binary({setClass, Class, Name}));
		false -> iter_match(Socket, Rest, Classes)
	end.
      

% Notifies the client if there is a class diagram, to highlight a specific class.
notify_class_diagram(Classes, Coordinator, Did, ClassDiagramId, Name) -> 
	  % If there is a class diagram 
  case ClassDiagramId of 
  % There is no class diagram.
   none -> none;
   % Catches all.	
   _ -> 
		% Gets the name of the class with it's Pid.
		Coordinator ! {class_diagram, ClassDiagramId, Did, highlight, getClass(find_class(Classes, Name), Name)}
	end.
  
find_class([{Socket, Name}| _Classes], Name) -> Socket;
find_class([_| Classes], Name) -> find_class(Classes, Name).

getName([], _Name) -> err;
getName([{_, Name}| _Names], Name) -> Name;
getName([_| Names], Name) -> getName(Names, Name).

% Gets the name of a specific node.
%getName(Socket)  ->
%	gen_tcp:send(Socket, term_to_binary({self(), getName})),
%	receive 
%	  {tcp, Socket, Info} ->
%	    case binary_to_term(Info) of
%		  {getName, Name} -> Name
%		
%	after
%		1000 ->
%		  err
%	end.

% Gets the class of a specific node.
getClass(Socket, Name) ->
	gen_tcp:send(term_to_binary({getClass, Name})),
	receive 
	  {tcp, Socket, Info} ->
	    case binary_to_term(Info) of
		  {getClass, Class} -> Class
		end
	after
		1000 ->
		  err
	end.
