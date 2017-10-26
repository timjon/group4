-module(executionlog).

%% API

-export([start_server/0]).
-define(Port, 9000).

start_server()->
	Pid = spawn_link(fun() ->
		{ok, LSocket} = gen_tcp:listen(?Port, [binary, {active, false}]),
		spawn(fun()-> acceptState(LSocket) end),
		timer:sleep(infinity)
		end),
	{ok, Pid}.	
		
acceptState(LSocket) ->
	{ok, ASocket} = gen_tcp:accept(LSocket),
	spawn(fun()-> acceptState(LSocket) end),
	handler(ASocket).

handler(ASocket)->
	inet:setopts(ASocket,[{active,once}]),
	receive
	{tcp,ASocket,<<"quit">>} ->
		gen_tcp:close(ASocket);
	{tcp,ASocket,<<"nodeSend">>} ->
		gen_tcp:send(ASocket,"Node sent a message"),
		handler(ASocket);
	{tcp,ASocket,<<"nodeReceive">>} ->
		gen_tcp:send(ASocket,"Node received a message"),
		handler(ASocket)	
		end.