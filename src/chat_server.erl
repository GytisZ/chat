-module(chat_server).

-behaviour(gen_server).

%% sort of public
-export([start_link/0, sign_in/2, sign_out/1, list_names/0, shutdown/0,
        send_message/2]).

%% not so public
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Server state
-record(state, {users=ets:new(users, [set, named_table])}).

%%%%%%%%%%%%%%%%%%%
%%% PUBLIC API %%%%
%%%%%%%%%%%%%%%%%%%


start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).


%% Try signing in. Server may refuse if the user name is taken
sign_in(Nick, Pid) ->
    gen_server:call({global, ?MODULE}, {sign_in, Nick, Pid}).


%% Sign out. This is asynchronous till client will be separated from the server
sign_out(Nick) ->
    gen_server:cast({global, ?MODULE}, {sign_out, Nick}).

%% Send message
send_message(Nick, Message) ->
    gen_server:call({global, ?MODULE}, {sendmsg, Nick, Message}).

%% Get the list of all the users currently connected to the server
list_names() ->
    gen_server:call({global, ?MODULE}, list_names).


%% Shut down the server
shutdown() ->
    gen_server:cast({global, ?MODULE}, stop).


%%% Server functions

init([]) ->
    {ok, #state{users=ets:new(users, [set, named_table])}}.


handle_call({sign_in, Nick, Pid}, _From, State = #state{users=_List}) ->
    case {ets:match(users, {Nick, '$1'}),
          ets:match(users, {'$1', Pid})} of
            {[],[]} -> 
                ets:insert(users, {Nick, Pid}),
                {reply, ok, State};
            {[], _} ->
                {reply, already_signed_in, State};
            {[_], _} ->
                {reply, name_taken, State}
    end;

handle_call(list_names, _From, State=#state{users=_List}) ->
    {reply, ets:match(users, {'$1', '_'}), State};

handle_call({sendmsg, To, Message}, _From, State=#state{users=_List}) ->
    case ets:match(users, {To, '$1'})  of
        [[Pid]]->
            Pid ! {printmsg, Message};
        [] -> ok
    end,
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({sign_out, Nick}, State=#state{users=_List}) ->
    ets:delete(users, Nick),
    {noreply, State}.


%% Placeholders for now
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(normal, _State) -> 
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
