-module(chat_server).

-behaviour(gen_server).

%% sort of public
-export([start_link/1, start_link/2, connect/2, network/1,
         list_names/1, list_channels/1, shutdown/1]).

%% not so public
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Server state
-record(state, {name=chat_server,
                max_users=50,
                map,
                node,
                leader,
                channels}).

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

list_channels(Server) ->
    gen_server:call({global, Server}, list_ch).

%% Connect Server to the Target server and its' cluster
connect(Server, Target) ->
    gen_server:cast({global, Server}, {connect, Target}).

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
    ChannelsTable = create_table(Server, "_ch"),
    ServersTable = create_table(Server, "_map"),
    ets:new(Server, [set, named_table]),
    ets:insert(ServersTable, {Server, node(), 50}),
    {ok, #state{name=Server,
                map=ServersTable,
                leader=Server,
                node=erlang:node(),
                channels=ChannelsTable}};

init([{Server, MaxUsers}]) ->
    ChannelsTable = create_table(Server, "_ch"),
    ServersTable = create_table(Server, "_map"),
    ets:new(Server, [set, named_table]),
    ets:insert(ServersTable, {Server, node(), MaxUsers}),
    {ok, #state{name=Server,
                node=erlang:node(),
                map=ServersTable,
                leader=Server,
                max_users=MaxUsers,
                channels=ChannelsTable}}.

handle_call({sign_in, Nick, Pid}, _From,  S=#state{name=Server, map=STbl}) ->
    Map = create_map(STbl),
    Forward = lists:delete([Server], Map),
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

handle_call({new_channel, Channel}, _From, S=#state{channels=ChTbl}) ->
    ets:insert(ChTbl, {Channel, []}),
    {reply, ok, S};

handle_call(list_names, _From, S=#state{name=Server}) ->
    {reply, ets:match(Server, {'$1', '_', '_'}), S};

handle_call(list_ch, _From, S=#state{channels=Ch}) ->
    {reply, ets:match(Ch, {'$1', '_'}), S};

handle_call({list_ch_users, Channel}, _From, S=#state{channels=ChTbl}) ->
    {reply, ets:match(ChTbl, {Channel, '$1'}), S};

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

handle_call({new_server, New, Node}, _From, S=#state{map=STbl}) ->
    ets:insert(STbl, {New, Node, 50}),
    erlang:monitor(process, {New, Node}),
    {reply, ok, S};

handle_call({new_join, {Name, Channel}}, _From, S=#state{channels=ChTbl}) ->
    CurrentUsers = ets:lookup_element(ChTbl, Channel, 2),
    NewUsers = lists:append(CurrentUsers, [Name]),
    ets:update_element(ChTbl, Channel, {2, NewUsers}),
    {reply, ok, S};

handle_call({new_leave, {Name, Channel}}, _From, S=#state{channels=ChTbl}) ->
    CurrentUsers = ets:lookup_element(ChTbl, Channel, 2),
    NewUsers = lists:delete(Name, CurrentUsers),
    ets:update_element(ChTbl, Channel, {2, NewUsers}),
    {reply, ok, S};

handle_call(network, _From, S=#state{name=Server, map=STbl, leader=Leader}) ->
    {reply, {Server, Leader, create_map(STbl)}, S};

handle_call(_Request, _From, S) ->
    {reply, ok, S}.

%% Create a new map with the connecting server appended and send it to
%% every other server on the network. Forward the request to the leader
%% if the server contacted wasn't the leader.

handle_cast({connect, Target}, S=#state{name=Server, node=Node}) ->
    gen_server:cast({global, Target}, {connect, Server, Node}),
    {noreply, S};

handle_cast({connect, New, Node}, S=#state{name=Server, 
                                           leader=Leader, 
                                           map=STbl}) ->
    Map = create_map(STbl),
    ets:insert(STbl, {New, Node, 50}),
    Recipients = lists:delete([Server], Map),
    case Leader == Server  of
        true -> 
            lists:map(fun([T]) -> new_server(New, Node,  T) end, Recipients), 
            gen_server:cast({global, New}, {init, ets:tab2list(STbl), Leader}),
            erlang:monitor(process, {New, Node}),
            {noreply, S};
        false -> gen_server:cast({global, New}, {leader, Leader}),
            {noreply, S}
    end;

handle_cast({init, NewMap, Leader}, S=#state{map=STbl}) ->
    ets:delete_all_objects(STbl),
    ets:insert(STbl, NewMap),
    Map = ets:match(STbl, {'$1', '$2', '_'}),
    lists:map(fun([Serv, Node]) -> 
                    erlang:monitor(process, {Serv, Node}) end, Map),
    {noreply, S#state{leader=Leader}};

handle_cast(stop, S) ->
    {stop, normal, S};

handle_cast({sign_out, Nick}, S=#state{map=STbl}) ->
    Map = create_map(STbl), 
    lists:map(fun([T]) ->
                gen_server:cast({global, T}, {user_left, Nick}) end, Map),
    {noreply, S};

handle_cast({user_left, Nick}, S=#state{name=Server}) ->
    ets:delete(Server, Nick),
    {noreply, S};

handle_cast({leader, Leader}, S=#state{name=Server}) ->
    connect(Server, Leader),
    {noreply, S};

handle_cast({create, Channel}, S=#state{name=Server,
                                        map=STbl,
                                        channels=ChTbl}) ->
    Map = create_map(STbl),
    Forward = lists:delete([Server], Map),
    case ets:match(ChTbl, {Channel, '_'}) of
        [] -> 
            ets:insert(ChTbl, {Channel, []}),
            lists:map(fun([Serv]) ->
                        new_channel(Channel, Serv) end, Forward),
            {noreply, S};
        [_] ->
            {noreply, S}
    end;

handle_cast({join, Name, Channel}, S=#state{name=Server, 
                                            map=STbl,
                                            channels=ChTbl}) ->
    Map = create_map(STbl),
    CurrentUsers = ets:lookup_element(ChTbl, Channel, 2),
    NewUsers = lists:append(CurrentUsers, [Name]),
    Forward = lists:delete([Server], Map),
    lists:map(fun([Serv]) ->
                new_join({Name, Channel}, Serv) end, Forward),
    ets:update_element(ChTbl, Channel, {2, NewUsers}),
    gen_server:cast({global, Server},
                    {send_ch, Name, Channel, " *** joined the channel ***"}),
    {noreply, S};

handle_cast({leave, Name, Channel}, S=#state{name=Server,
                                             map=STbl,
                                             channels=ChTbl}) ->
    gen_server:cast({global, Server},
                    {send_ch, Name, Channel, "*** has left the channel ***"}),
    Map = create_map(STbl),
    CurrentUsers = ets:lookup_element(ChTbl, Channel, 2),
    NewUsers = lists:delete(Name, CurrentUsers),
    Forward = lists:delete([Server], Map),
    lists:map(fun([Serv]) ->
                new_leave({Name, Channel}, Serv) end, Forward),
    ets:update_element(ChTbl, Channel, {2, NewUsers}),
    {noreply, S};

handle_cast({send_ch, From, Ch, Msg}, S=#state{name=Server, 
                                               channels=ChTbl}) ->
    ChannelUsers = ets:lookup_element(ChTbl, Ch, 2),
    lists:map(fun(U) -> 
                send_msg(Server, U, {ch, From, Ch, Msg}) end, ChannelUsers),
    {noreply, S};

handle_cast({msg, To, Message}, S=#state{name=Server}) ->
    [[Pid, Host]] = ets:match(Server, {To, '$1', '$2'}),
    case Host of 
        Server ->
            Pid ! {msg, Message};
        Other ->
           send_msg(Other, To, Message)
    end,
    {noreply, S}.
    
handle_info({'DOWN', _, process, {Name, _Node}, _}, S=#state{map=STbl,
                                                             leader=Leader}) ->
    ets:delete(STbl, Name),
    Map = create_map(STbl),
    case Name == Leader of
        true -> SortedMap = lists:sort(Map),
            [[NewLeader] | _ ] = SortedMap;
        false -> NewLeader = Leader
    end,
    {noreply, S#state{leader=NewLeader}};


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

new_server(New, Node, Target) ->
    gen_server:call({global, Target}, {new_server, New, Node}).

new_user(New, [Target]) ->
    gen_server:call({global, Target}, {new_user, New}).

new_channel(New, Target) ->
    gen_server:call({global, Target}, {new_channel, New}).

new_join(New, Target) ->
    gen_server:call({global, Target}, {new_join, New}).

new_leave(New, Target) ->
    gen_server:call({global, Target}, {new_leave, New}).

send_msg(Server, User, Message) ->
    gen_server:cast({global, Server}, {msg, User, Message}).

create_table(Server, Suffix) ->
    NewTable = list_to_atom(atom_to_list(Server) ++ Suffix),
    ets:new(NewTable, [set, named_table]),
    NewTable.
 
create_map(TableName) ->
    ets:match(TableName, {'$1', '_', '_'}).

