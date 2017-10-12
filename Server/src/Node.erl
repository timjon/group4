-module(node).

-export([init/1]).

init(Coordinator) ->
  loop(Coordinator).

loop(Coordinator) ->
  receive
    {send_message, Message, Receiver} ->
      Coordinator ! {send_reply, Message, self(), Receiver},
      loop(Coordinator);
    {receive_message, Message, Sender} ->
      Coordinator ! {receive_reply, Message, Sender, self()},
      loop(Coordinator)
  end.
