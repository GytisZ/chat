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

client_crash_test_() ->
    {"server should detect client crashes.",
     ?setup(fun client_crash/1)}.

create_channel_test_() ->
    {"client can create chat channels.",
     ?setup(fun create_channel/1)}.

list_channels_test_() ->
    {"client can see a list of channels.",
     ?setup(fun list_channels/1)}.

join_channel_test_() ->
    {"client can join a channel.",
     ?setup(fun join_channel/1)}.

send_channel_test_() ->
    {"client can send messages to a channel.",
     ?setup(fun send_channel/1)}.

%% ===================================================================== 
%% Setup functions
%% =====================================================================  

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
    List1 = chat_client:list_names(baliulia),
    chat_client:sign_out(baliulia),
    timer:sleep(50),
    List2 = chat_client:list_names(baliulia),
    [?_assertEqual([["baliulia"]], List1),
     ?_assertEqual([], List2)]. 

name_taken(_) ->
    chat_client:start(gytis),
    chat_client:name(foobar, gytis, "Gytis"),
    chat_client:start(imposter),
    Response = chat_client:name(foobar, imposter, "Gytis"),
    chat_client:shutdown(gytis),
    chat_client:shutdown(imposter),
    [?_assertEqual(name_taken, Response)].

mult_user(_) ->
    chat_client:start(nifnif),
    chat_client:start(nufnuf),
    chat_client:start(nafnaf),
    chat_client:name(foobar, nifnif, "nifnif"),
    chat_client:name(foobar, nafnaf, "nafnaf"),
    List1 = chat_client:list_names(nifnif),
    chat_client:name(foobar, nufnuf, "nufnuf"),
    List2 = chat_client:list_names(nifnif),
    chat_client:sign_out(nufnuf),
    timer:sleep(50),
    List3 = chat_client:list_names(nifnif),
    chat_client:sign_out(nafnaf),
    chat_client:sign_out(nifnif),
    timer:sleep(50),
    List4 = chat_client:list_names(nifnif),
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
     
client_crash(_) ->
    chat_client:start(phantom),
    chat_client:name(foobar, phantom, "Phantom"),
    chat_client:shutdown(phantom),
    [?_assertEqual([], chat_server:list_names(foobar))].

create_channel(_) ->
    chat_client:start(gytis),
    chat_client:name(foobar, gytis, "Gytis"),
    chat_client:create(gytis, erlang),
    Channels = chat_server:list_channels(foobar),
    chat_client:shutdown(gytis),
    [?_assertEqual([[erlang]], Channels)].

list_channels(_) ->
    chat_client:start(foo),
    chat_client:name(foobar, foo, "Bar"),
    chat_client:create(foo, erlang),
    chat_client:create(foo, haskell),
    Channels = chat_client:list_channels(foo),
    chat_client:shutdown(foo),
    [?_assertEqual([[haskell], [erlang]], Channels)].

join_channel(_) ->
    chat_client:start(foo),
    chat_client:name(foobar, foo, "Bar"),
    chat_client:create(foo, erlang),
    chat_client:join(foo, erlang),
    Users = chat_client:list_ch_users(foo, erlang),
    chat_client:shutdown(foo),
    [?_assertEqual([[["Bar"]]], Users)].

send_channel(_) ->
    chat_client:start(foo),
    chat_client:name(foobar, foo, "Bar"),
    chat_client:create(foo, kanalas),
    chat_client:join(foo, kanalas),
    chat_client:send_channel(foo, kanalas, "Hello world"),
    chat_client:shutdown(foo),
    [?_assertEqual(1, 1)].
    
%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
