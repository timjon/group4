%% @author Kosara
%% @doc @todo Separates client and server implementation.


-module(node_client).

%% ====================================================================
%% API functions
%% ====================================================================
-export([write/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================
write(Msg) ->
	node_gen_server:write(Msg).