-module(fuml_super_sup).
-behaviour(supervisor).
-export([start_link/0, stop/0, start_fuml/3]).
-export([init/1])

start_link() ->
  supervisor:start_link({local, fuml}, ?MODULE, []).
  
%If you want to kill the server :)
stop() ->
  case whereis(fuml) of 
    P when is_pid(P) -> exit(P, kill);
	
	_                -> ok
	end.
	
init([]) ->
  MaxRestart = 6,
  MaxTime = 3600,
  {ok, {{one_for_one, MaxRestart, MaxTime}, []}}.
  
start_fuml(Name, Limit, MFA) ->
  ChildSpec = {Name,
  {fuml_sup, start_link, [Name, Limit, MFA]},
  permanent, 10500, supervisor, [fuml_sup]},
  supervisor:start_child(fuml, ChildSpec).
  
stop_fuml(Name) ->
supervisor:terminate_child(fuml, Name),
supervisor:delete_child(fuml, Name).