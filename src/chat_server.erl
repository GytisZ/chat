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

%% ===================================================================== 
%% Public API
%% ===================================================================== 

%% --------------------------------------------------------------------- 
%% @doc
%% Start server, optional second argument sets user limit
%% @end
%% --------------------------------------------------------------------- 
start_link(Server) ->
    {ok, Pid}=gen_server:start_link({global, Server}, ?MODULE, [{Server}], []),
    register(Server, Pid),
    {ok, Pid}.

start_link(Server, MaxUsers) ->
    gen_server:start_link({global, Server}, ?MODULE, 
                          [Server, MaxUsers], []).

%% --------------------------------------------------------------------- 
%% @doc
%% Get the list of all the users currently connected to the cluster
%% @end
%% --------------------------------------------------------------------- 
list_names(Server) ->
    gen_server:call({global, Server}, list_names).

%% --------------------------------------------------------------------- 
%% @doc
%% Get the list of all active channels
%% @end
%% --------------------------------------------------------------------- 
list_channels(Server) ->
    gen_server:call({global, Server}, list_ch).

%% --------------------------------------------------------------------- 
%% @doc
%% Connect Server to the Target server and its' cluster
%% @end
%% --------------------------------------------------------------------- 
connect(Server, Target) ->
    gen_server:cast({global, Server}, {connect, Target}).

%% --------------------------------------------------------------------- 
%% @doc
%% Show who is the current leader and what servers are online
%% @end
%% --------------------------------------------------------------------- 
network(Server) ->
    gen_server:call({global, Server}, network).

%% --------------------------------------------------------------------- 
%% @doc
%% Shut down the server
%% @end
%% --------------------------------------------------------------------- 
shutdown(Server) ->
    gen_server:call({global, Server}, stop).

%% ===================================================================== 
%% gen_server callbacks
%% ===================================================================== 

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

handle_call(list_names, _From, S=#state{name=Server}) ->
    {reply, ets:match(Server, {'$1', '_', '_'}), S};

handle_call(list_ch, _From, S=#state{channels=Ch}) ->
    {reply, ets:match(Ch, {'$1', '_'}), S};

handle_call({list_ch_users, Channel}, _From, S=#state{channels=ChTbl}) ->
    {reply, ets:match(ChTbl, {Channel, '$1'}), S};

handle_call({sendmsg, From, To, Msg}, _From, S=#state{name=Server}) ->
    [[Author]] = ets:match(Server, {'$1', From, '_'}),
    case ets:match(Server, {To, '_', '$2'})  of
        [[Host]]->
            gen_server:cast({global, Host}, {msg, To, {priv, Author, Msg}});
        [] -> gen_server:cast(From, {not_found, To})
    end,
    {reply, ok, S};

handle_call(stop, _From, S) ->
    {stop, normal, ok, S};

handle_call(network, _From, S=#state{name=Server, map=STbl, leader=Leader}) ->
    {reply, {Server, Leader, create_map(STbl)}, S};

handle_call(_Request, _From, S) ->
    {reply, ok, S}.

handle_cast({nickserv, Nick, Pid}, S=#state{leader=Leader}) ->
    gen_server:cast({global, Leader}, {sign_in, Nick, Pid}),
    {noreply, S};

handle_cast({sign_in, Nick, Pid}, S=#state{name=Server, map=STbl}) ->
    Map = create_map(STbl),
    Forward = lists:delete([Server], Map),
    case {ets:match(Server, {Nick, '$1', '_'}),
          ets:match(Server, {'$1', Pid, '_'})} of
        {[],[]} -> 
            ets:insert(Server, {Nick, Pid, Server}),
            lists:map(fun([Serv]) ->
                        new(user, {Nick, Pid, Server}, Serv) end, Forward),
            erlang:monitor(process, Pid),
            {noreply, S};
        {[], _} ->
            Pid ! {msg, already_signed_in},
            {noreply, S};
        {[_], _} ->
            Pid ! {msg, {name_taken, Nick}},
            {noreply, S}
    end;

handle_cast({user, NewUser}, S=#state{name=Server}) ->
    ets:insert(Server, NewUser),
    {noreply, S};



%% ---------------------------------------------------------------------  
%% @private
%% @doc
%% Create a new map with the connecting server appended and send it to
%% every other server on the network. Forward the request to the leader
%% if the server contacted wasn't the leader.
%% @end
%% --------------------------------------------------------------------- 
handle_cast({connect, Target}, S=#state{name=Server,
                                       node=Node,
                                       max_users=MaxUsers}) ->
    gen_server:cast({global, Target}, {connect, Server, Node, MaxUsers}),
    {noreply, S};

handle_cast({connect, New, Node, MaxUsers}, S=#state{name=Server, 
                                                    leader=Leader, 
                                                    map=STbl,
                                                    channels=ChTbl}) ->
    Map = create_map(STbl),
    ets:insert(STbl, {New, Node, MaxUsers}),
    Recipients = lists:delete([Server], Map),
    case Leader == Server  of
        true -> 
            lists:map(fun([T]) -> 
                       new(server, {New, Node, MaxUsers}, T) end, Recipients), 
            PassMap = ets:tab2list(STbl),
            PassChannels = ets:tab2list(ChTbl),
            PassUsers = ets:tab2list(Server),
            Pass = {PassUsers, PassMap, PassChannels},
            gen_server:cast({global, New}, {init, Pass, Leader}),
            erlang:monitor(process, {New, Node}),
            {noreply, S};
        false -> gen_server:cast({global, New}, {leader, Leader}),
            {noreply, S}
    end;

handle_cast({init, New, Leader}, S=#state{name=Server,
                                         map=STbl,
                                         channels=ChTbl}) ->
    {NewUsers, NewMap, NewChannels} = New,
    ets:delete_all_objects(STbl),
    ets:insert(STbl, NewMap),
    ets:delete_all_objects(Server),
    ets:insert(Server, NewUsers),
    ets:delete_all_objects(ChTbl),
    ets:insert(ChTbl, NewChannels),
    Map = ets:match(STbl, {'$1', '$2', '_'}),
    lists:map(fun([Serv, Node]) -> 
                erlang:monitor(process, {Serv, Node}) end, Map),
    {noreply, S#state{leader=Leader}};

handle_cast({server, {New, Node, MaxUsers}}, S=#state{map=STbl}) ->
    ets:insert(STbl, {New, Node, MaxUsers}),
    erlang:monitor(process, {New, Node}),
    {noreply, S};

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

handle_cast({chanserv, Channel}, S=#state{leader=Leader}) ->
    gen_server:cast({global, Leader}, {create, Channel}),
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
                        new(channel, Channel, Serv) end, Forward),
            {noreply, S};
        [_] ->
            {noreply, S}
    end;

handle_cast({channel, Channel}, S=#state{channels=ChTbl}) ->
    ets:insert(ChTbl, {Channel, []}),
    {noreply, S};

handle_cast({join, Name, Channel}, S=#state{name=Server, 
                                            map=STbl,
                                            channels=ChTbl}) ->
    Map = create_map(STbl),
    CurrentUsers = ets:lookup_element(ChTbl, Channel, 2),
    NewUsers = lists:append(CurrentUsers, [Name]),
    Forward = lists:delete([Server], Map),
    lists:map(fun([Serv]) ->
                new(new_join, {Name, Channel}, Serv) end, Forward),
    ets:update_element(ChTbl, Channel, {2, NewUsers}),
    gen_server:cast({global, Server},
                    {send_ch, Name, Channel, " *** joined the channel ***"}),
    {noreply, S};

handle_cast({new_join, {Name, Channel}}, S=#state{channels=ChTbl}) ->
    CurrentUsers = ets:lookup_element(ChTbl, Channel, 2),
    NewUsers = lists:append(CurrentUsers, [Name]),
    ets:update_element(ChTbl, Channel, {2, NewUsers}),
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
                new(new_leave, {Name, Channel}, Serv) end, Forward),
    ets:update_element(ChTbl, Channel, {2, NewUsers}),
    {noreply, S};

handle_cast({new_leave, {Name, Channel}}, S=#state{channels=ChTbl}) ->
    CurrentUsers = ets:lookup_element(ChTbl, Channel, 2),
    NewUsers = lists:delete(Name, CurrentUsers),
    ets:update_element(ChTbl, Channel, {2, NewUsers}),
    {noreply, S};

handle_cast({send_ch, From, Ch, Msg}, S=#state{name=Server, 
                                               channels=ChTbl}) ->
    ChannelUsers = ets:lookup_element(ChTbl, Ch, 2),
    lists:map(fun(U) -> 
                send_msg(Server, U, {ch, From, Ch, Msg}) end, ChannelUsers),
    {noreply, S};

handle_cast({quit, Pid}, S=#state{name=Server}) ->
    case ets:match(Server, {'$1', Pid, '_'}) of
        [[Nick]] -> 
            ets:delete(Server, Nick);
        _ -> ok
    end,
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

%% --------------------------------------------------------------------- 
%% @private
%% @doc
%% Elects new leader and updates network map in case one of the servers
%% goes down or is shut down.
%% @end
%% --------------------------------------------------------------------- 
handle_info({'DOWN', _, process, {Name, _Node}, _}, S=#state{name=Server,
                                                            map=STbl,
                                                            leader=Leader}) ->
    ets:delete(STbl, Name),
    AffectedUsers = ets:match(Server, {'_', '$1', Name}),
    Map = create_map(STbl),
    lists:map(fun([Pid]) -> 
        lists:map(fun([Serv]) ->
            gen_server:cast({global, Serv}, {quit, Pid}) end, Map) end, 
        AffectedUsers),
    case Name == Leader of
        true -> SortedMap = lists:sort(Map),
            [[NewLeader] | _ ] = SortedMap;
        false -> NewLeader = Leader
    end,
    {noreply, S#state{leader=NewLeader}};

%% --------------------------------------------------------------------- 
%% @private
%% @doc
%% Informs all servers that the user has disconnected.
%% @end
%% --------------------------------------------------------------------- 
handle_info({'DOWN', _MRef, process, Pid, _}, S=#state{map=STbl}) ->
    Map = create_map(STbl),
    lists:map(fun([Serv]) -> 
                gen_server:cast({global, Serv}, {quit, Pid}) end, Map),
    {noreply, S};

handle_info(_Msg, S) ->
    {noreply, S}.

terminate(normal, _S) -> 
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.


%% ===================================================================== 
%% Internal functions
%% ===================================================================== 
%% --------------------------------------------------------------------- 
%% @private
%% @doc
%% Sends a message about new user/server/channel/join... to a specified
%% server.
%% @end
%% --------------------------------------------------------------------- 
new(Atom, Message, Target) ->
    gen_server:cast({global, Target}, {Atom, Message}).

send_msg(Server, User, Message) ->
    gen_server:cast({global, Server}, {msg, User, Message}).

%% --------------------------------------------------------------------- 
%% @private
%% @doc
%% Takes server name, adds the specified suffix to it and creates an
%% ETS table with that name.
%% @end
%% --------------------------------------------------------------------- 
create_table(Server, Suffix) ->
    NewTable = list_to_atom(atom_to_list(Server) ++ Suffix),
    ets:new(NewTable, [set, named_table]),
    NewTable.

create_map(TableName) ->
    ets:match(TableName, {'$1', '_', '_'}).

