-module(chat_client_tests).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).
%%% TEST DESCRIPTIIONS

signin_and_name_list_test_() ->
     {"Sign in/out is working and name list is kept up to date.",
     ?setup(fun sign_in_and_out/1)}.

same_nick_test_() ->
    {"name_taken is returned if the nick is already take.",
     ?setup(fun name_taken/1)}.

%%% SETUP FUNCTIONS

start() ->
    {ok, Pid} = chat_server:start_link(),
    Pid.

stop(Pid) ->
    MRef = erlang:monitor(process, Pid),
    chat_server:shutdown(),
    receive {'DOWN', MRef, _, _, _} -> ok end.
%%% ACTUAL TESTS

sign_in_and_out(_) ->
    chat_client:start(),
    chat_client:name("baliulia"),
    chat_client:name("buddha"),
    List1 = chat_client:list_names(),
    chat_client:sign_out(),
    List2 = chat_client:list_names(),
    [?_assertEqual(["baliulia", "buddha"], List1),
     ?_assertEqual(["baliulia"], List2)]. 

name_taken(_) ->
    chat_client:name("Gytis"),
    [?_assertEqual(name_taken, chat_client:name("Gytis"))].
