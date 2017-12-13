-module(nodecoordinator).
-export([init/1]).

%%Author: Tim Jonasson
%%Version: 1.0

%Initializes the nodecoordinator
init(Socket) -> 
  inet:setopts(Socket, [{active, true}]),
  loop(Socket, []).
  
loop(Socket, Nodes) ->
  receive
    {tcp, Socket, Info} ->
	  case binary_to_term(Info) of
	    {spawn_node, Did, Name} -> 
		  Self = self(),
		  Binary = term_to_binary({Did, print_information, ["Spawned node " ++ atom_to_list(Name)]}),
		  gen_tcp:send(Socket, Binary),
		  node:init(Self), 
		  loop(Socket, [{Name, node:init(Self)}| Nodes]);
		  
	    {send_message, From, To, Message, Message_number} -> 
		  Pid = find_node(Nodes, From),
		  Pid ! {send_message, From, To, Message, Message_number},
		  loop(Socket, Nodes);
		
		{receive_message, From, To, Message, Message_number} ->
		  Pid = find_node(Nodes, To),
		  Pid ! {receive_message, From, To, Message, Message_number},
		  loop(Socket, Nodes)
	  end;
	  
	{tcp_closed, _Socket} -> 
	  shut_down;
	  
	Message -> 
	  gen_tcp:send(Socket, term_to_binary(Message)),
	  loop(Socket, Nodes)
  end.
  
find_node([{Name, Pid}|_], Name) -> Pid;
find_node([_| Nodes], Name) -> find_node(Nodes, Name).
