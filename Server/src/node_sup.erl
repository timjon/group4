-module(node_sup).
-import(node, [init/2]).
-export([start_link_sup/2]).

start_link_sup(Coordinator, Class_name) ->
  P = self(),
  Sup = spawn_link(fun () -> init_sup(P, Coordinator, Class_name) end),
  
  receive ack -> {ok, Sup} end.
  
init_sup(P, Coordinator, Class_name) ->
  process_flag(trap_exit, true),
  ok = start_link_supervised(Coordinator, Class_name),
  P ! ack,
  loop_sup(Coordinator, Class_name).

loop_sup(Coordinator, Class_name) ->
  receive
    {'EXIT', _From, normal} ->
      ok;
    {'EXIT', _From, _Reason} ->
      ok = start_link_supervised(Coordinator, Class_name),
      loop_sup(Coordinator, Class_name) 
  end.

start_link_supervised(Coordinator, Class_name) -> 
  case whereis(Class_name) of
    undefined -> spawn_link(fun () -> node:init(Coordinator, Class_name)end), ok;
	
	_         -> ok
	end.