-module(chat_server).

-behaviour(gen_server).

%% sort of public
-export([start_link/1, start_link/2, sign_in/3, sign_out/2, connect/2, map/1,
         list_names/1, shutdown/1, send_message/3]).

%% not so public
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Server state
-record(state, {name=chat_server, max_users=50, map=sets:new(), 
                users=ets:new(users, [set, named_table])}).

%%%%%%%%%%%%%%%%%%%
%%% PUBLIC API %%%%
%%%%%%%%%%%%%%%%%%%


start_link(ServerName) ->
    {ok, Pid} = gen_server:start_link({global, ServerName},
                                      ?MODULE, [{ServerName}], []),
    gen_server:call({global, ServerName}, init_map),
    {ok, Pid}.

start_link(ServerName, MaxUsers) ->
    gen_server:start_link({global, ServerName}, ?MODULE,
                          [{ServerName, MaxUsers}], []),
    gen_server:call({global, ServerName}, init_map).


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

connect(ServerName, TargetServer) ->
    gen_server:call({global, ServerName}, {connect, TargetServer}).

map(ServerName) ->
    erlang:display(gen_server:call({global, ServerName}, showmap)),
    gen_server:call({global, ServerName}, show_map).

%% Shut down the server
shutdown(ServerName) ->  gen_server:call({global, ServerName}, stop).

%%%%%%%%%%%%%%%%%
%%% CALLBACKS %%%
%%%%%%%%%%%%%%%%%

init([{Server}]) ->
    {ok, #state{name=Server, map=sets:new(),
                users=ets:new(Server, [set, named_table])}};

init([{Server, MaxUsers}]) ->
    {ok, #state{name=Server, max_users=MaxUsers, map=sets:new(),
                users=ets:net(Server, [set, named_table])}}.

handle_call(init_map, _From, S=#state{name=Server, map=Map}) ->
    {reply, ok, S#state{map=sets:add_element(Server, Map)}};

handle_call({sign_in, Nick, Pid}, _From, 
            State = #state{name=Server, users=List}) ->
    case {ets:match(List, {Nick, '$1', '_'}),
          ets:match(List, {'$1', Pid, '_'})} of
            {[],[]} -> 
                ets:insert(List, {Nick, Pid, Server}),
                erlang:monitor(process, Pid),
                {reply, ok, State};
            {[], _} ->
                {reply, already_signed_in, State};
            {[_], _} ->
                {reply, name_taken, State}
    end;

handle_call(list_names, _From, State=#state{users=List}) ->
    {reply, ets:match(List, {'$1', '_', '_'}), State};

handle_call({sendmsg, From, To, Message}, _From, State=#state{users=List}) ->
    case ets:match(List, {To, '$1', '_'})  of
        [[Pid]]->
            [[Author]] = ets:match(List, {'$1', From, '_'}),
            Pid ! {printmsg, Author, Message};
        [] -> gen_server:cast(From, {not_found, To})
    end,
    {reply, ok, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({connect, TargetServer}, _From, State=#state{name=Server}) ->
    gen_server:call({global, TargetServer}, {connecting, Server}),
    {reply, ok, State};

handle_call({connecting, ChildServer}, _From, State=#state{map=Map}) ->
    {reply, ok, State#state{map=sets:add_element(ChildServer, Map)}}; 

handle_call(show_map, _From, S=#state{map=Map}) ->
    {reply, sets:to_list(Map), S};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({sign_out, Nick}, State=#state{users=List}) ->
    ets:delete(List, Nick),
    {noreply, State};

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info({'DOWN', _MRef, process, Pid, _}, State=#state{users=List}) ->
    case ets:match(List, {'$1', Pid, '_'}) of
        [[Nick]] -> 
            ets:delete(List, Nick)
    end,
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(normal, _State) -> 
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
