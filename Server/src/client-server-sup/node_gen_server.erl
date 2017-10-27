%% @author Kosara
%% @doc @todo Server implemented using gen_server.
%% Starting the supervisor also starts the server.


-module(node_gen_server).
-behaviour(gen_server).

-export([start_link/0, stop/0, write/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% start_link/3
%% ====================================================================
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% stop/1
%% ====================================================================
stop() ->
	gen_server:cast({local, ?MODULE}, stop).

%% write/1
%% Simple printing function to test out the distributed functionality.
%% ====================================================================
write(Msg) ->
	io:format("Process received: ~p~n", [Msg]).
	

%% ====================================================================
%% Callback functions
%% ====================================================================

%% init/1
%% ====================================================================
init(_Args) ->
	%% Lets us know when the parent shuts down.
    process_flag(trap_exit, true),
	io:format("~p (~p) starting... ~n", [{local, ?MODULE}, self()]),
    {ok, []}.


%% handle_call/3
%% Handles exchanges with the JInterface.
%% ====================================================================
handle_call(_Request, _From, State) ->
    {reply, i_dont_know, State}.



%% handle_cast/2
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% Called when a timeout occurs.
%% ====================================================================
handle_info(Info, State) ->
	io:format("Unexpected message: ~p~n", [Info]),
    {noreply, State}.


%% terminate/2
%% ====================================================================
terminate(normal, _State) ->
	%% Debugging message.
	io:format("Node terminated, ref. ~p~n", [{local, ?MODULE}]),
    ok;
terminate(_reason, _State) ->
	%io:format("Node terminated for reason: ~p~n", [Reason])
	io:format("terminating ~p~n", [{local, ?MODULE}]),
	ok.
	

%% code_change/3
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
	%% No change planned.
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


