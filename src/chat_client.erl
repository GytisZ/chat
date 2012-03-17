-module(chat_client).
-behaviour(gen_server).

%% Public
-export([start/0, start/1, name/1, name/2, send/2, list_names/0, sign_out/0,
        sign_out/1]).

%% Usual OTP goodness
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% State
-record(state, {name, pid}).
%%%%%%%%%%%%%%%%%%
%%% PUBLIC API %%%
%%%%%%%%%%%%%%%%%%

start() ->
    {ok, Pid} = gen_server:start({local, ?MODULE}, ?MODULE, [], []),
    set_pid(Pid).
start(RefName) ->
    {ok, Pid} = gen_server:start({local, RefName}, ?MODULE, [], []),
    set_pid(RefName, Pid).

    
name(Nick) ->
    gen_server:call(?MODULE, {sign_in, Nick}).
name(RefName, Nick) ->
    gen_server:call(RefName, {sign_in, Nick}).

send(To, Message) ->
    chat_server:send_message(To, Message).

list_names() ->
    chat_server:list_names().

sign_out() ->
    gen_server:cast(?MODULE, sign_out).
sign_out(RefName) ->
    gen_server:cast(RefName, sign_out).
%%%%%%%%%%%%%%%%%
%%% CALLBACKS %%%
%%%%%%%%%%%%%%%%%

init([]) ->
    {ok, #state{}}.


handle_call({sign_in, Name}, _From, State=#state{pid=Pid}) ->
    case chat_server:sign_in(Name, Pid) of
        ok -> {reply, ok, State#state{name=Name}};
        name_taken -> 
            io:format("~p is taken. Select a different nick.~n", [Name]),
            {reply, name_taken, State};
        already_signed_in ->
            io:format("You are already signed in.~n", []),
            {reply, already_signed_in, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(sign_out, State=#state{name=Nick}) ->
    chat_server:sign_out(Nick),
    {noreply, State};

handle_cast({set_pid, Pid}, State=#state{}) ->
    {noreply, State#state{pid=Pid}};

handle_cast(_Message, State) ->
    {noreply, State}.


handle_info({printmsg, Message}, State) ->
    io:format("You received: ~p~n", [Message]),
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

set_pid(Pid) ->
    gen_server:cast(?MODULE, {set_pid, Pid}).
set_pid(RefName, Pid) ->
    gen_server:cast(RefName, {set_pid, Pid}).
