%% @author Kosara
%% @doc @todo Node supervisor. Restarts the server when it crashes.


-module(node_gen_server_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% start_link/1
%% ====================================================================
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% init/1
%% ====================================================================
init([]) ->
	io:format("~p (~p) starting... ~n", [{local, ?MODULE}, self()]),
	
	%% Restart strategy only restarts the node that crashed.
	%% Number of maximum restarts: 1, seconds between restarts: 60.
	%% Type of child: worker.

    AChild = {'AName', {'node_gen_server', start_link, []},
	     permanent, 1000, worker, ['node_gen_server']},
    {ok, {{one_for_one, 1, 60}, [AChild]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


