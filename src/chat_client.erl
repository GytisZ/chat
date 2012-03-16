-module(chat_client).

-export([sign_in/1, sign_out/1, list_names/0, handle_messages/1]).


sign_in(Nick) ->
    Pid = spawn(chat_client, handle_messages, [Nick]),
    chat_server:sign_in(Nick, Pid).

sign_out(Nick) ->
    chat_server:sign_out(Nick).

list_names() ->
    chat_server:list_names().

handle_messages(Nick) ->
   receive
       {printmsg, Message} ->
            io:format("~p got: ~p~n", [Nick, Message]),
            handle_messages(Nick);
       stop ->
            ok
   end.

            
