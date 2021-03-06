-module(chat_server_tests).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TEST DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

start_stop_test_() ->
    {"The server can be started, stopped and has a registered name.",
     ?setup(fun is_registered/1)}.

server_connect_test_() ->
    {"Servers can connect each other.",
     ?setup(fun server_connect/1)}.

multiple_server_test_() ->
    {"Multiple servers can connect and disconnect.",
     ?setup(fun multiple_servers/1)}.

users_on_other_servers_test_() ->
    {"User tables should be updated on all servers.",
     ?setup(fun user_table_updates/1)}.

channels_propagate_test_() ->
    {"Channels propagate accross the cluster.",
     ?setup(fun channels_propagate/1)}.

network_channel_test_() ->
    {"channels are global.",
     ?setup(fun network_channel/1)}.

wrong_messages_test_() ->
    {"server controls unplanned messages correctly",
     ?setup(fun wrong_messages/1)}.

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    {ok, Pid} = chat_server_sup:start_link(),
    {ok, Pid2} = chat_server_sup:start(foo),
    {Pid, Pid2}.

stop({Pid, _}) ->
    MRef = erlang:monitor(process, Pid),
    chat_server_sup:stop(foo),
    erlang:exit(Pid, normal),
    receive {'DOWN', MRef, _, _, _} -> ok end,
    timer:sleep(200).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

is_registered({_, Pid}) ->
    [?_assert(erlang:is_process_alive(Pid))].

server_connect(_) ->
    chat_server_sup:start(bar),
    chat_server:connect(foo, bar),
    Channels = chat_server:network(bar),
    [?_assertEqual({bar, bar, [[foo], [bar]]}, Channels)].

multiple_servers(_) ->
    chat_server_sup:start(man),
    chat_server_sup:start(chu),
    chat_server:connect(man, chu),
    timer:sleep(50),
    chat_server:connect(foo, man),
    timer:sleep(50),
    Snapshot1 = chat_server:network(foo),
    Snapshot2 = chat_server:network(chu),
    Snapshot3 = chat_server:network(man),
    chat_server_sup:stop(chu),
    timer:sleep(50),
    Snapshot4 = chat_server:network(man),
    chat_server_sup:stop(man),
    [?_assertEqual({foo, chu, [[foo], [man], [chu]]}, Snapshot1),
     ?_assertEqual({chu, chu, [[foo], [man], [chu]]}, Snapshot2),
     ?_assertEqual({man, chu, [[foo], [man], [chu]]}, Snapshot3),
     ?_assertEqual({man, foo, [[foo], [man]]}, Snapshot4)].

user_table_updates(_) ->
    chat_server:start_link(man),
    chat_server:start_link(chu),
    chat_server:connect(man, chu),
    timer:sleep(50),
    chat_server:connect(foo, man),
    timer:sleep(50),
    chat_client:start(jonas),
    chat_client:start(petras),
    chat_client:start(karolis),
    chat_client:sign_in(jonas, foo, "Jonas"),
    chat_client:sign_in(petras, chu, "Petras"),
    chat_client:sign_in(karolis, man, "Karolis"),
    List1 = chat_server:list_names(foo),
    List2 = chat_server:list_names(man),
    List3 = chat_server:list_names(chu),
    [?_assertEqual([["Petras"], ["Jonas"], ["Karolis"]], List1),
     ?_assertEqual([["Petras"], ["Jonas"], ["Karolis"]], List2),
     ?_assertEqual([["Petras"], ["Jonas"], ["Karolis"]], List3)].

channels_propagate(_) ->
    chat_server:start_link(bar),
    chat_server:start_link(baz),
    chat_server:connect(foo, bar),
    timer:sleep(50),
    chat_server:connect(baz, foo),
    chat_client:start(qux),
    chat_client:sign_in(qux, foo, "Quux"),
    chat_client:create(qux, erlang),
    chat_client:create(qux, irssi),
    chat_client:join(qux, erlang),
    timer:sleep(50),
    chat_client:start(kinzaza),
    chat_client:sign_in(kinzaza, baz, "kinzaza"),
    chat_client:join(kinzaza, erlang),
    chat_client:send_channel(kinzaza, erlang, "yo"),
    chat_client:leave(kinzaza, erlang),
    Channels = chat_client:list_channels(kinzaza),
    chat_client:shutdown(kinzaza),
    chat_client:shutdown(qux),
    gen_server:cast({global, bar}, stop),
    chat_server:shutdown(baz),
    [?_assertEqual([[irssi], [erlang]], Channels)].

network_channel(_) ->
    chat_server:start_link(bar),
    chat_server:connect(foo, bar),
    timer:sleep(50),
    chat_client:start(baz),
    chat_client:sign_in(baz, foo, "baz"),
    chat_client:start(qux),
    chat_client:sign_in(qux, bar, "qux"),
    chat_client:create(baz, baras),
    chat_client:create(baz, viktorina),
    chat_client:join(baz, baras),
    timer:sleep(50),
    chat_client:join(baz, viktorina),
    timer:sleep(50),
    chat_client:join(qux, baras),
    timer:sleep(50),
    List1 = chat_client:list_ch_users(baz, baras),
    List2 = chat_client:list_ch_users(qux, viktorina),
    chat_client:shutdown(baz),
    chat_client:shutdown(qux),
    chat_server:shutdown(bar),
    [?_assertEqual([[["baz", "qux"]]], List1),
     ?_assertEqual([[["baz"]]], List2)].

wrong_messages({_, Pid}) ->
    Pid ! helloworld,
    [?_assertEqual(gen_server:cast(Pid, yabadabadoo), ok)].
