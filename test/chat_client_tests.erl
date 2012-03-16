-module(chat_client_tests).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).
%%% TEST DESCRIPTIIONS

signin_and_name_list_test_() ->
     {"Sign in/out is working and name list is kept up to date.",
     ?setup(fun sign_in_and_out/1)}.

%%% SETUP FUNCTIONS

start() ->
    {ok, Pid} = chat_server:start_link(),
    Pid.

stop(_) ->
    chat_server:shutdown().

%%% ACTUAL TESTS

sign_in_and_out(_) ->
    chat_client:sign_in("baliulia"),
    chat_client:sign_in("buddha"),
    List1 = chat_client:list_names(),
    chat_server:sign_out("baliulia"),
    List2 = chat_client:list_names(),
    [?_assertEqual(["baliulia", "buddha"], List1),
     ?_assertEqual(["buddha"], List2)]. 
