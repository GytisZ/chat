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

use_client_without_ref_test_() ->
    {"chat client should be fully functional without passing the RefName to the
      API functions",
     ?setup(fun mini_api/1)}.

wrong_messages_test_() ->
    {"chat client shouldn't crash because it received a wrong message.",
     ?setup(fun wrong_messages/1)}.

client_sup_test_() ->
    {"testing untested chat_client_sup functions.",
     ?setup(fun client_sup/1)}.

%% ===================================================================== 
%% Setup functions
%% =====================================================================  

start() ->
    {ok, Pid} = chat_server_sup:start_link(),
    chat_server_sup:start(foobar),
    {ok, Pid2} = chat_client_sup:start_link(),
    {Pid, Pid2}.

stop({Pid, Pid2}) ->
    MRef = erlang:monitor(process, Pid),
    MRef2 = erlang:monitor(process, Pid2),
    erlang:exit(Pid, normal),
    receive {'DOWN', MRef, _, _, _} -> ok end,
    erlang:exit(Pid2, normal),
    receive {'DOWN', MRef2, _, _, _} -> ok end.

%%% ACTUAL TESTS

sign_in_and_out(_) ->
    chat_client_sup:start(baliulia),
    chat_client:sign_in(baliulia, foobar, "baliulia"),
    List1 = chat_client:list_names(baliulia),
    chat_client:sign_out(baliulia),
    timer:sleep(50),
    List2 = chat_client:list_names(baliulia),
    chat_server:shutdown(foobar),
    timer:sleep(50),
    [?_assertEqual([["baliulia"]], List1),
     ?_assertEqual([], List2)]. 

name_taken(_) ->
    chat_client_sup:start(gytis),
    chat_client:sign_in(gytis, foobar, "Gytis"),
    chat_client_sup:start(imposter),
    chat_client:sign_in(imposter, foobar, "Gytis"),
    List = chat_server:list_names(foobar),
    chat_client_sup:stop(gytis),
    chat_client_sup:stop(imposter),
    [?_assertEqual([["Gytis"]], List)].

mult_user(_) ->
    chat_client:start(nifnif),
    chat_client:start(nufnuf),
    chat_client:start(nafnaf),
    chat_client:sign_in(nifnif, foobar, "nifnif"),
    chat_client:sign_in(nafnaf, foobar, "nafnaf"),
    List1 = chat_client:list_names(nifnif),
    chat_client:sign_in(nufnuf, foobar, "nufnuf"),
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
    chat_client:sign_in(newton, foobar, "Isaac"),
    chat_client:sign_in(newton, foobar, "Hotpants"),
    List = chat_server:list_names(foobar),
    [?_assertEqual([["Isaac"]], List)].

send_msg(_) ->
    chat_client:start(r2d2),
    chat_client:start(c3po),
    chat_client:sign_in(r2d2, foobar, "R2D2"),
    chat_client:sign_in(c3po, foobar, "C3PO"),
    [?_assertEqual(ok,
                   chat_client:send(c3po, "R2D2", "Robotas - irgi zmogus."))].

non_existant_nick(_) ->
    chat_client:start(airhead),
    chat_client:sign_in(airhead, foobar, "name"),
    [?_assertEqual(ok, chat_client:send(airhead, "friend", "message"))].
     
client_crash(_) ->
    chat_client_sup:start(phantom),
    chat_client:sign_in(phantom, foobar, "Phantom"),
    chat_client_sup:stop(phantom),
    [?_assertEqual([], chat_server:list_names(foobar))].

create_channel(_) ->
    chat_client_sup:start(gytis),
    chat_client:sign_in(gytis, foobar, "Gytis"),
    chat_client:create(gytis, erlang),
    Channels = chat_server:list_channels(foobar),
    chat_client_sup:stop(gytis),
    [?_assertEqual([[erlang]], Channels)].

list_channels(_) ->
    chat_client_sup:start(foo),
    chat_client:sign_in(foo, foobar, "Bar"),
    chat_client:create(foo, erlang),
    chat_client:create(foo, haskell),
    Channels = chat_client:list_channels(foo),
    chat_client_sup:stop(foo),
    [?_assertEqual([[haskell], [erlang]], Channels)].

join_channel(_) ->
    chat_client_sup:start(foo),
    chat_client:sign_in(foo, foobar, "Bar"),
    chat_client:create(foo, erlang),
    chat_client:join(foo, erlang),
    Users = chat_client:list_ch_users(foo, erlang),
    chat_client:leave(foo, erlang),
    Users2 = chat_client:list_ch_users(foo, erlang),
    chat_client_sup:stop(foo),
    [?_assertEqual([[["Bar"]]], Users),
     ?_assertEqual([[[]]], Users2)].

send_channel(_) ->
    chat_client_sup:start(foo),
    chat_server_sup:start(barfoo),
    chat_server:connect(barfoo, foobar),
    timer:sleep(50),
    chat_client_sup:start(bar),
    chat_client:sign_in(foo, foobar, "Bar"),
    chat_client:sign_in(bar, barfoo, "Foo"),
    chat_client:create(foo, kanalas),
    timer:sleep(50),
    chat_client:join(foo, kanalas),
    timer:sleep(50),
    chat_client:join(bar, kanalas),
    timer:sleep(50),
    chat_client:send_channel(foo, kanalas, "Hello world"),
    timer:sleep(50),
    chat_client_sup:stop(foo),
    chat_client_sup:stop(bar),
    chat_server:shutdown(barfoo),
    [?_assertEqual(1, 1)].
    
mini_api(_) ->
    chat_client_sup:start(),
    chat_client:sign_in(foobar, "Baz"),
    chat_client:send("Baz", "Hey me"),
    List1 = chat_client:list_names(),
    chat_client:create(erlang),
    List2 = chat_client:list_channels(),
    chat_client:join(erlang),
    List3 = chat_client:list_ch_users(erlang),
    chat_client:send_channel(erlang, "Hey me again"),
    chat_client:leave(erlang),
    List4 = chat_client:list_ch_users(erlang),
    chat_client:sign_out(),
    chat_client:shutdown(),
    [?_assertEqual([["Baz"]], List1),
     ?_assertEqual([[erlang]], List2),
     ?_assertEqual([[["Baz"]]], List3),
     ?_assertEqual([[[]]], List4)].

wrong_messages(_) ->
    {ok, Pid} = chat_client_sup:start(foo),
    gen_server:cast(Pid, somerandomstuff),
    Pid ! {morerandomstuff},
    [?_assertEqual(ok, ok)].

client_sup(_) ->
    chat_client_sup:start(),
    [?_assertEqual(chat_client_sup:stop(), ok)].
