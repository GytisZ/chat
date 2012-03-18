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


handle_call({sign_in, Server, Name}, _From, State=#state{pid=Pid}) ->
    case chat_server:sign_in(Server, Name, Pid) of
        ok -> {reply, ok, State#state{server=Server, name=Name}};
        name_taken -> 
            io:format("~p is taken. Select a different nick.~n", [Name]),
            {reply, name_taken, State};
        already_signed_in ->
            io:format("You are already signed in.~n", []),
            {reply, already_signed_in, State}
    end;

handle_call({sendmsg, To, Message}, _From, State=#state{server=S, pid=Pid}) ->
    gen_server:call({global, S}, {sendmsg, Pid, To, Message}),
    {reply, ok, State};

handle_call(list_names, _From, State=#state{server=Server}) ->
    {reply, chat_server:list_names(Server), State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(sign_out, State=#state{server=Server, name=Nick}) ->
    chat_server:sign_out(Server, Nick),
    {noreply, State};

handle_cast({set_pid, Pid}, State=#state{}) ->
    {noreply, State#state{pid=Pid}};

handle_cast({not_found, To}, State) ->
    io:format("~p - no such user.", [To]),
    {noreply, State};

handle_cast(_Message, State) ->
    {noreply, State}.


handle_info({printmsg, From, Message}, State) ->
    io:format("~p says: ~p~n", [From, Message]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

set_pid(RefName, Pid) ->
    gen_server:cast(RefName, {set_pid, Pid}).
