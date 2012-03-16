-module(chat_client).

-export([sign_in/1, sign_out/1, list_names/0]).


sign_in(Nick) ->
%    Pid = spawn(chat_client, handle_messsages, [Nickname]),
    chat_server:sign_in(Nick).

sign_out(Nick) ->
    chat_server:sign_out(Nick).

list_names() ->
    chat_server:list_names().
