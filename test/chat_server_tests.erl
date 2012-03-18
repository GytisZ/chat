-module(chat_server_tests).
-include_lib("eunit/include/eunit.hrl").

%%% TEST DESCRIPTIONS

start_stop_test_() ->
    {"The server can be started, stopped and has a registered name.",
     {setup,
      fun start/0,
      fun stop/1,
      fun is_registered/1}}.

%%% SETUP FUNCTIONS

start() ->
    {ok, Pid} = chat_server:start_link(foobar),
    Pid.

stop(Pid) ->
    MRef = erlang:monitor(process, Pid),
    chat_server:shutdown(foobar),
    receive {'DOWN', MRef, _, _, _} -> ok end.

%%% ACTUAL TESTS
is_registered(Pid) ->
    [?_assert(erlang:is_process_alive(Pid))].
