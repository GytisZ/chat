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

send_private_msg_test_() ->
    {"chat_server handles private messages.",
     ?setup(fun send_msg/1)}.

non_existant_nick_test_() ->
    {"message author is informed of undelivered message.",
     ?setup(fun non_existant_nick/1)}.

%%% SETUP FUNCTIONS

start() ->
    {ok, Pid} = chat_server:start_link(foobar),
    Pid.

stop(Pid) ->
    MRef = erlang:monitor(process, Pid),
    chat_server:shutdown(foobar),
    receive {'DOWN', MRef, _, _, _} -> ok end.
%%% ACTUAL TESTS

sign_in_and_out(_) ->
    chat_client:start(baliulia),
    chat_client:name(foobar, baliulia, "baliulia"),
    List1 = chat_client:list_names(foobar),
    chat_client:sign_out(baliulia),
    List2 = chat_client:list_names(foobar),
    [?_assertEqual([["baliulia"]], List1),
     ?_assertEqual([], List2)]. 

name_taken(_) ->
    chat_client:start(gytis),
    chat_client:name(foobar, gytis, "Gytis"),
    chat_client:start(imposter),
    [?_assertEqual(name_taken, chat_client:name(foobar, imposter, "Gytis"))].

mult_user(_) ->
    chat_client:start(nifnif),
    chat_client:start(nufnuf),
    chat_client:start(nafnaf),
    chat_client:name(foobar, nifnif, "nifnif"),
    chat_client:name(foobar, nafnaf, "nafnaf"),
    List1 = chat_client:list_names(foobar),
    chat_client:name(foobar, nufnuf, "nufnuf"),
    List2 = chat_client:list_names(foobar),
    chat_client:sign_out(nufnuf),
    List3 = chat_client:list_names(foobar),
    chat_client:sign_out(nafnaf),
    chat_client:sign_out(nifnif),
    List4 = chat_client:list_names(foobar),
    [?_assertEqual([["nifnif"], ["nafnaf"]], List1),
     ?_assertEqual([["nufnuf"], ["nifnif"], ["nafnaf"]], List2),
     ?_assertEqual([["nifnif"], ["nafnaf"]], List3),
     ?_assertEqual([], List4)].

sign_in_twice(_) ->
    chat_client:start(newton),
    chat_client:name(foobar, newton, "Isaac"),
    [?_assertEqual(already_signed_in, 
                   chat_client:name(foobar, newton, "Hotpants"))].

send_msg(_) ->
    chat_client:start(r2d2),
    chat_client:start(c3po),
    chat_client:name(foobar, r2d2, "R2D2"),
    chat_client:name(foobar, c3po, "C3PO"),
    [?_assertEqual(ok,
                   chat_client:send(c3po, "R2D2", "Robotas - irgi zmogus."))].

non_existant_nick(_) ->
    chat_client:start(airhead),
    chat_client:name(foobar, airhead, "name"),
    [?_assertEqual(ok, chat_client:send(airhead, "friend", "message"))].
     
%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
