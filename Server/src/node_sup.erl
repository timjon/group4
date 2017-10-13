-module(node_sup).
-import(node, [init/2]).
-export([start_link_sup/1, send/1, receive_message/1, crash/1]).

start_link_sup(Class_name) ->
  P = self(),
  Sup = spawn_link(fun () -> init_sup(P, Class_name) end),
  
  receive ack -> {ok, Sup} end.
  
init_sup(P, Class_name) ->
  process_flag(trap_exit, true),
  ok = start_link_supervised(P, Class_name),
  P ! ack,
  loop_sup(P, Class_name).

loop_sup(Coordinator, Class_name) ->
  receive
    {'EXIT', _From, normal} ->
      ok;
    {'EXIT', _From, _Reason} ->
	  io:format("restarting ~n"),
      start_link_sup(Class_name);
	{'DOWN', _, process, _Pid, _Reason} ->
	  io:format("Fuck this shit ~n"),
	  start_link_sup(Class_name);
	X  -> 
	  io:format("Hello ~n"),
	  io:format(X)
  end.

start_link_supervised(Coordinator, Class_name) -> 
  case whereis(Class_name) of
    undefined -> spawn_link(fun () -> node:init(Coordinator, Class_name)end), ok;
	_         -> ok2
	end.
	
	
send(Receiver) ->
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
  
crash(Receiver) ->
  Receiver ! {crash_message}.