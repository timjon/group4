-module(nodecoordinator).
-export().

%%Author: Tim Jonasson
%%Version: 1.0

%Initializes the nodecoordinator
init(Socket) -> 
  inet:setopts(Socket, [{active, true}]),
  loop(Socket).
  
loop(Socker) ->
  receive
    {tcp, Socket, Info} ->
	  binary_to_term(Info),
	  loop(Socket);
	  
	{tcp_closed, _Socket} -> 
	  shut_down;
	  
	Message -> 
	  gen_tcp:send(Socket, term_to_binary(Message)),
	  loop(Socket)
  end
  
  