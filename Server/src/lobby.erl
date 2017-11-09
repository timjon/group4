-module(lobby).
-export([init/1]).

init(Creator_Socket) -> loop(Creator_Socket, [Creator_Socket], []).


loop(Creator_Socket, Members, Diagrams) -> 
  receive
    {remove_lobby, Creator_Socket} -> ok;
	{create_diagram, Creator_Socket, {Did, Class_names, Classes, Messages}} -> not_implemented;
	{command, {Did, next_message}} -> not_implemented;
	{command, {Did, previous_message}} -> not_implemented;
  end.