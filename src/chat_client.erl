-module(chat_client).
-behaviour(gen_server).

%% Public
-export([start/1, name/3, send/3, list_names/1, sign_out/1, shutdown/1]).

%% Usual OTP goodness
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% State
-record(state, {name, pid, server}).
%%%%%%%%%%%%%%%%%%
%%% PUBLIC API %%%
%%%%%%%%%%%%%%%%%%

start(RefName) ->
    {ok, Pid} = gen_server:start({local, RefName}, ?MODULE, [], []),
    set_pid(RefName, Pid).

    
name(ServerName, RefName, Nick) ->
    gen_server:call(RefName, {sign_in, ServerName, Nick}).

send(RefName, To, Message) ->
    gen_server:call(RefName, {sendmsg, To, Message}).

list_names(RefName) ->
    gen_server:call(RefName, list_names).

sign_out(RefName) ->
    gen_server:cast(RefName, sign_out).

shutdown(RefName) ->
    gen_server:call(RefName, stop).
%%%%%%%%%%%%%%%%%
%%% CALLBACKS %%%
%%%%%%%%%%%%%%%%%

init([]) ->
    {ok, #state{}}.


handle_call({sign_in, Server, Name}, _From, S=#state{pid=Pid}) ->
    case gen_server:call({global, Server}, {sign_in, Name, Pid}) of
        ok -> {reply, ok, S#state{server=Server, name=Name}};
        name_taken -> 
            io:format("~p is taken. Select a different nick.~n", [Name]),
            {reply, name_taken, S};
        already_signed_in ->
            io:format("You are already signed in.~n", []),
            {reply, already_signed_in, S}
    end;

handle_call({sendmsg, To, Message}, _From, S=#state{server=S, pid=Pid}) ->
    gen_server:call({global, S}, {sendmsg, Pid, To, Message}),
    {reply, ok, S};

handle_call(list_names, _From, S=#state{server=Server}) ->
    {reply, chat_server:list_names(Server), S};

handle_call(stop, _From, S) ->
    {stop, normal, ok, S};

handle_call(_Request, _From, S) ->
    {reply, ok, S}.


handle_cast(sign_out, S=#state{server=Server, name=Nick}) ->
    gen_server:cast({global, Server}, {sign_out, Nick}),
    {noreply, S};

handle_cast({set_pid, Pid}, S=#state{}) ->
    {noreply, S#state{pid=Pid}};

handle_cast({not_found, To}, S) ->
    io:format("~p - no such user.", [To]),
    {noreply, S};

handle_cast(_Message, S) ->
    {noreply, S}.


handle_info({printmsg, From, Message}, S) ->
    io:format("~p says: ~p~n", [From, Message]),
    {noreply, S};

handle_info(_Info, S) ->
    {noreply, S}.

terminate(_Reason, _S) ->
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.


%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

set_pid(RefName, Pid) ->
    gen_server:cast(RefName, {set_pid, Pid}).
