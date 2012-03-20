-module(chat_server_tests).
-include_lib("eunit/include/eunit.hrl").

%%% TEST DESCRIPTIONS

start_stop_test_() ->
    {"The server can be started, stopped and has a registered name.",
     {setup,
      fun start/0,
      fun stop/1,
      fun is_registered/1}}.

server_connect_test_() ->
    {"Servers can connect each other.",
     {setup,
      fun start/0,
      fun stop/1,
      fun server_connect/1}}.

%%% SETUP FUNCTIONS

start() ->
    {ok, Pid} = chat_server:start_link(foo),
    Pid.

stop(Pid) ->
    MRef = erlang:monitor(process, Pid),
    chat_server:shutdown(foo),
    receive {'DOWN', MRef, _, _, _} -> ok end.

%%% ACTUAL TESTS
is_registered(Pid) ->
    [?_assert(erlang:is_process_alive(Pid))].

server_connect(_) ->
    chat_server:start_link(bar),
    chat_server:connect(foo, bar),
    [?_assertEqual({bar, bar, [foo, bar]}, chat_server:network(bar))].
