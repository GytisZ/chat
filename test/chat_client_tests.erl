-module(chat_client_tests).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).
%%% TEST DESCRIPTIIONS

signin_and_name_list_test_() ->
     {"Sign in/out is working and name list is kept up to date.",
     ?setup(fun sign_in_and_out/1)}.

same_nick_test_() ->
    {"name_taken is returned if the nick is already taken.",
     ?setup(fun name_taken/1)}.

multiple_user_test_() ->
    {"chat_server keeps track of multiple sign in/outs.",
     ?setup(fun mult_user/1)}.

sign_in_twice_test_() ->
    {"chat server prohibits a second sign in attempt.",
     ?setup(fun sign_in_twice/1)}.

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
    List1 = chat_client:list_names(),
    chat_client:sign_out(),
    List2 = chat_client:list_names(),
    [?_assertEqual([["baliulia"]], List1),
     ?_assertEqual([], List2)]. 

name_taken(_) ->
    chat_client:start(gytis),
    chat_client:name(gytis, "Gytis"),
    chat_client:start(imposter),
    [?_assertEqual(name_taken, chat_client:name(imposter, "Gytis"))].

mult_user(_) ->
    chat_client:start(nifnif),
    chat_client:start(nufnuf),
    chat_client:start(nafnaf),
    chat_client:name(nifnif, "nifnif"),
    chat_client:name(nafnaf, "nafnaf"),
    List1 = chat_client:list_names(),
    chat_client:name(nufnuf, "nufnuf"),
    List2 = chat_client:list_names(),
    chat_client:sign_out(nufnuf),
    List3 = chat_client:list_names(),
    chat_client:sign_out(nafnaf),
    chat_client:sign_out(nifnif),
    List4 = chat_client:list_names(),
    [?_assertEqual([["nifnif"], ["nafnaf"]], List1),
     ?_assertEqual([["nufnuf"], ["nifnif"], ["nafnaf"]], List2),
     ?_assertEqual([["nifnif"], ["nafnaf"]], List3),
     ?_assertEqual([], List4)].

sign_in_twice(_) ->
    chat_client:start(newton),
    chat_client:name(newton, "Isaac"),
    [?_assertEqual(already_signed_in, chat_client:name(newton, "Hotpants"))].

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
