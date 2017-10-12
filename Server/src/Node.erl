-module(node).

-export([init/2]).

init(Coordinator, Class_name) ->
  P = spawn_link(fun () ->  loop(Coordinator) end),
  register(Class_name, P).

loop(Coordinator) ->
  receive
    {send_message, Message, Receiver} ->
      Coordinator ! {send_reply, Message, self(), Receiver},
      loop(Coordinator);
    {receive_message, Message, Sender} ->
      Coordinator ! {receive_reply, Message, Sender, self()},
      loop(Coordinator)
  end.
