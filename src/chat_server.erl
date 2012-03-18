-module(chat_server).

-behaviour(gen_server).

%% sort of public
-export([start_link/1, start_link/2, sign_in/3, sign_out/2,
         list_names/1, shutdown/1, send_message/3]).

%% not so public
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Server state
-record(state, {name=chat_server, max_users=50, 
                users=ets:new(users, [set, named_table])}).

%%%%%%%%%%%%%%%%%%%
%%% PUBLIC API %%%%
%%%%%%%%%%%%%%%%%%%


start_link(ServerName) ->
    gen_server:start_link({global, ServerName}, ?MODULE, [], []).

start_link(ServerName, MaxUsers) ->
    gen_server:start_link({global, ServerName}, ?MODULE, [MaxUsers], []).


%% Try signing in. Server may refuse if the user name is taken
sign_in(ServerName, Nick, Pid) ->
    gen_server:call({global, ServerName}, {sign_in, Nick, Pid}).


%% Sign out. This is asynchronous till client will be separated from the server
sign_out(ServerName, Nick) ->
    gen_server:cast({global, ServerName}, {sign_out, Nick}).

%% Send message
send_message(ServerName, Nick, Message) ->
    gen_server:call({global, ServerName}, {sendmsg, Nick, Message}).

%% Get the list of all the users currently connected to the server
list_names(ServerName) ->
    gen_server:call({global, ServerName}, list_names).


%% Shut down the server
shutdown(ServerName) ->
    gen_server:cast({global, ServerName}, stop).


%%% Server functions

init([]) ->
    {ok, #state{users=ets:new(users, [set, named_table])}};

init([MaxUsers]) ->
    {ok, #state{max_users=MaxUsers, users=ets:net(users, [set, named_table])}};

init([ServerName, MaxUsers]) ->
    {ok, #state{name=ServerName, max_users=MaxUsers,
                users=ets:net(users, [set, named_table])}}.

handle_call({sign_in, Nick, Pid}, _From, 
            State = #state{name=Server, users=_List}) ->
    case {ets:match(users, {Nick, '$1', '_'}),
          ets:match(users, {'$1', Pid, '_'})} of
            {[],[]} -> 
                ets:insert(users, {Nick, Pid, Server}),
                {reply, ok, State};
            {[], _} ->
                {reply, already_signed_in, State};
            {[_], _} ->
                {reply, name_taken, State}
    end;

handle_call(list_names, _From, State=#state{users=_List}) ->
    {reply, ets:match(users, {'$1', '_', '_'}), State};

handle_call({sendmsg, From, To, Message}, _From, State=#state{users=_List}) ->
    case ets:match(users, {To, '$1', '_'})  of
        [[Pid]]->
            [[Author]] = ets:match(users, {'$1', From, '_'}),
            Pid ! {printmsg, Author, Message};
        [] -> gen_server:cast(From, {not_found, To})
    end,
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({sign_out, Nick}, State=#state{users=_List}) ->
    ets:delete(users, Nick),
    {noreply, State};

handle_cast(_Message, State) ->
    {noreply, State}.


%% Placeholders for now
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(normal, _State) -> 
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
