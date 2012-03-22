-module(chat_server).

-behaviour(gen_server).

%% sort of public
-export([start_link/1, start_link/2, connect/2, network/1,
         list_names/1, shutdown/1]).

%% not so public
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Server state
-record(state, {name=chat_server,
                max_users=50,
                map,
                leader}).

%%%%%%%%%%%%%%%%%%%
%%% PUBLIC API %%%%
%%%%%%%%%%%%%%%%%%%


start_link(Server) ->
    {ok, Pid}=gen_server:start_link({global, Server}, ?MODULE, [{Server}], []),
    register(Server, Pid),
    {ok, Pid}.

start_link(Server, MaxUsers) ->
    gen_server:start_link({global, Server}, ?MODULE, 
                          [Server, MaxUsers], []).

%% Get the list of all the users currently connected to the server
list_names(Server) ->
    gen_server:call({global, Server}, list_names).

%% Connect Server to the Target server and its' cluster
connect(Server, Target) ->
    gen_server:cast({global, Target}, {connect, Server}).

%% Show who is the current leader and what servers are online
network(Server) ->
    gen_server:call({global, Server}, network).

%% Shut down the server
shutdown(Server) ->
    gen_server:call({global, Server}, stop).

%%%%%%%%%%%%%%%%%
%%% CALLBACKS %%%
%%%%%%%%%%%%%%%%%

init([{Server}]) ->
    ets:new(Server, [set, named_table]),
    {ok, #state{name=Server,
                map=[Server],
                leader=Server}};

init([{Server, MaxUsers}]) ->
    ets:new(Server, [set, named_tabled]),
    {ok, #state{name=Server,
                map=[Server],
                leader=Server,
                max_users=MaxUsers}}.

handle_call({sign_in, Nick, Pid}, _From,  S=#state{name=Server, map=Map}) ->
    Forward = lists:delete(Server, Map),
    case {ets:match(Server, {Nick, '$1', '_'}),
          ets:match(Server, {'$1', Pid, '_'})} of
        {[],[]} -> 
            ets:insert(Server, {Nick, Pid, Server}),
            lists:map(fun(Serv) ->
                        new_user({Nick, Pid, Server}, Serv) end, Forward),
            erlang:monitor(process, Pid),
            {reply, ok, S};
        {[], _} ->
            {reply, already_signed_in, S};
        {[_], _} ->
            {reply, name_taken, S}
    end;

handle_call({new_user, NewUser}, _From, S=#state{name=Server}) ->
    ets:insert(Server, NewUser),
    {reply, ok, S};


handle_call(list_names, _From, S=#state{name=Server}) ->
    {reply, ets:match(Server, {'$1', '_', '_'}), S};

handle_call({sendmsg, From, To, Message}, _From, S=#state{name=Server}) ->
    [[Author]] = ets:match(Server, {'$1', From, '_'}),
    case ets:match(Server, {To, '$1', '$2'})  of
        [[Pid, Server]]->
            Pid ! {printmsg, Author, Message};
        [[_, Host]] -> 
            gen_server:call({global, Host}, {sendmsg, From, To, Message});
        [] -> gen_server:cast(From, {not_found, To})
    end,
    {reply, ok, S};

handle_call(stop, _From, S) ->
    {stop, normal, ok, S};

handle_call({new_server, New}, _From, S=#state{map=Map}) ->
    NewMap = lists:append([New], Map),
    erlang:monitor(process, New),
    {reply, ok, S#state{map=NewMap}};

handle_call({init, NewMap, Leader}, _From, S=#state{}) ->
    lists:map(fun(Serv) -> erlang:monitor(process, Serv) end, NewMap),
    {reply, ok, S#state{map=NewMap, leader=Leader}};

handle_call(network, _From, S=#state{name=Server, map=Map, leader=Leader}) ->
    {reply, {Server, Leader, Map}, S};

handle_call(_Request, _From, S) ->
    {reply, ok, S}.

%% Create a new map with the connecting server appended and send it to
%% every other server on the network. Forward the request to the leader
%% if the server contacted wasn't the leader.

handle_cast({connect, New}, S=#state{name=Server, leader=Leader, map=Map}) ->
    NewMap = lists:append([New], Map),
    Recipients = lists:delete(Server, Map),
    case Leader == Server  of
        true -> lists:map(fun(T) ->  new_server(New, T) end, Recipients), 
            gen_server:call({global, New}, {init, NewMap, Leader}),
            erlang:monitor(process, New),
            {noreply, S#state{map=NewMap}};
        false -> gen_server:cast({global, New}, {leader, Leader}),
            {noreply, S}
    end;

handle_cast(stop, S) ->
    {stop, normal, S};

handle_cast({sign_out, Nick}, S=#state{map=Map}) ->
    lists:map(fun(T) ->
                gen_server:cast({global, T}, {user_left, Nick}) end, Map),
    {noreply, S};

handle_cast({user_left, Nick}, S=#state{name=Server}) ->
    ets:delete(Server, Nick),
    {noreply, S};

handle_cast({leader, Leader}, S=#state{name=Server}) ->
    connect(Server, Leader),
    {noreply, S};

handle_cast(_Message, S) ->
    {noreply, S}.

handle_info({'DOWN', _, process, {Name, _Node}, _}, S=#state{map=Map,
                                                             leader=Leader}) ->
    NewMap = lists:delete(Name, Map),
    case Name == Leader of
        true -> SortedMap = lists:sort(NewMap),
            [NewLeader | _ ] = SortedMap;
        false -> NewLeader = Leader
    end,
    {noreply, S#state{map=NewMap, leader=NewLeader}};


handle_info({'DOWN', _MRef, process, Pid, _}, S=#state{name=Server}) ->
    case ets:match(Server, {'$1', Pid, '_'}) of
        [[Nick]] -> 
            ets:delete(Server, Nick);
        true -> ok
    end,
    {noreply, S};

handle_info(_Msg, S) ->
    {noreply, S}.

terminate(normal, _S) -> 
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.


%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

new_server(New, Target) ->
    gen_server:call({global, Target}, {new_server, New}).

new_user(New, Target) ->
    gen_server:call({global, Target}, {new_user, New}).
