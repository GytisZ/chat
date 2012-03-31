-module(chat_server).

-behaviour(gen_server).

%% public
-export([start_link/1, connect/2, network/1,
         list_names/1, list_channels/1, shutdown/1]).

%% private
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Server state
-record(state, {name=chat_server,
                map,
                pid,
                leader,
                channels}).

%% ===================================================================== 
%% Public API
%% ===================================================================== 

%% --------------------------------------------------------------------- 
%% @doc
%% Start server
%% @end
%% --------------------------------------------------------------------- 
start_link(Server) ->
    gen_server:start_link({global, Server}, ?MODULE, [{Server}], []).

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
    ets:insert(ServersTable, {Server, self()}),
    {ok, #state{name=Server,
                map=ServersTable,
                leader=Server,
                pid=self(),
                channels=ChannelsTable}}.

%% --------------------------------------------------------------------- 
%% @private
%% @doc
%% Callback for list_names/1, gets user names from user table and
%% returns them
%% @end
%% --------------------------------------------------------------------- 
handle_call(list_names, _From, S=#state{name=Server}) ->
    {reply, ets:match(Server, {'$1', '_', '_'}), S};

%% --------------------------------------------------------------------- 
%% @private
%% @doc
%% Callback for list_channels/1, returns a list of all current channels
%% @end
%% --------------------------------------------------------------------- 
handle_call(list_ch, _From, S=#state{channels=Ch}) ->
    {reply, ets:match(Ch, {'$1', '_'}), S};

%% --------------------------------------------------------------------- 
%% @private
%% @doc
%% Callback for chat_client:list_ch_users/1,2. Finds the appropriate
%% table entry and returns it.
%% @end
%% --------------------------------------------------------------------- 
handle_call({list_ch_users, Channel}, _From, S=#state{channels=ChTbl}) ->
    {reply, ets:match(ChTbl, {Channel, '$1'}), S};

%% --------------------------------------------------------------------- 
%% @private
%% @doc
%% Callback for chat_client:sign_in/2,3. Forwards the request to the
%% cluster leader in order to improve consistency.
%% @end
%% --------------------------------------------------------------------- 
handle_call({nickserv, Nick, Pid}, From, S=#state{leader=Leader}) ->
    NewUser = {Nick, Pid},
    gen_server:cast({global, Leader}, {sign_in, NewUser, From}),
    {noreply, S};

%% --------------------------------------------------------------------- 
%% @private
%% @doc
%% Callback for chat_client:send/2,3. Matches the addresssee's name to
%% the corresponding user's entry, looks up his host and forwards the
%% message to the corresponding server. 
%% Adds the authors name instead of pid to the message.
%%
%% Notifies the chat_client that addressee was not_found if there is
%% no matching entry.
%% @end
%% --------------------------------------------------------------------- 
handle_call({sendmsg, From, To, Msg}, _From, S=#state{name=Server}) ->
    [[Author]] = ets:match(Server, {'$1', From, '_'}),
    case ets:match(Server, {To, '_', '$2'})  of
        [[Host]]->
            send_msg(Host, To, {priv, Author, Msg});
        [] -> gen_server:cast(From, {not_found, To})
    end,
    {reply, ok, S};

handle_call(stop, _From, S) ->
    {stop, normal, ok, S};

%% --------------------------------------------------------------------- 
%% @private
%% @doc
%% Callback for network/1, Builds a tuple {current_server_name,
%% cluster_leader_name, [list_of_all_servers_in_the_cluser}.
%% @end
%% --------------------------------------------------------------------- 
handle_call(network, _From, S=#state{name=Server, map=STbl, leader=Leader}) ->
    {reply, {Server, Leader, create_map(STbl)}, S};

handle_call(_Request, _From, S) ->
    {noreply, S}.



%% --------------------------------------------------------------------- 
%% @private
%% @doc
%% Only the cluster leader uses this one. Other servers forward requests
%% to here in their handle_call({nickserv, ...}, ...) callback. 
%%
%% Checks if the new client is already signed in or the name it chose
%% is taken. If not - chooses a random server, inserts the new user
%% there and tells other servers to do the same.
%% @end
%% --------------------------------------------------------------------- 
handle_cast({sign_in, {Nick, Pid}, From}, S=#state{name=Leader, map=STbl}) ->
    %% chooses a random server to assign the user to
    Network = ets:tab2list(STbl),
    Rand = random:uniform(length(Network)),
    {Server, _} = lists:nth(Rand, Network),

    %% creates a list of servers that are gonna get messages about the
    %% new user
    ServerList = create_map(STbl),
    Forward = lists:delete([Leader], ServerList),

    %% tries matching the new client both by nick and by pid to find
    %% out if the nick is taken or if the client is already signed in
    case {ets:match(Leader, {Nick, '$1', '_'}),
          ets:match(Leader, {'$1', Pid, '_'})} of
        {[],[]} ->              
            %%  all good, insert new user into the table and tell
            %%  others to do the same
            ets:insert(Leader, {Nick, Pid, Server}),
            lists:map(fun([Serv]) ->
                        new(user, {Nick, Pid, Server}, Serv) end, Forward),
            erlang:monitor(process, Pid),
            gen_server:reply(From, {server, Server}),
            {noreply, S};
        {[], _} ->
            gen_server:reply(From, already_signed_in),
            {noreply, S};
        {[_], _} ->
            gen_server:reply(From, name_taken),
            {noreply, S}
    end;

%% --------------------------------------------------------------------- 
%% @private
%% @doc
%% Receives the forwarded info about new users on all non-leader servers
%% @end
%% --------------------------------------------------------------------- 
handle_cast({user, NewUser}, S=#state{name=Server}) ->
    ets:insert(Server, NewUser),
    {noreply, S};

%% --------------------------------------------------------------------- 
%% @private
%% @doc
%% Forwards all servers info about a user leaving
%% @end
%% --------------------------------------------------------------------- 
handle_cast({sign_out, Nick}, S=#state{map=STbl}) ->
    ServerList = create_map(STbl), 
    lists:map(fun([T]) ->
                gen_server:cast({global, T}, {user_left, Nick}) end, 
              ServerList),
    {noreply, S};

%% --------------------------------------------------------------------- 
%% @private
%% @doc
%% Deletes user's who has left entry from the user table
%% --------------------------------------------------------------------- 
handle_cast({user_left, Nick}, S=#state{name=Server}) ->
    ets:delete(Server, Nick),
    {noreply, S};



%% --------------------------------------------------------------------- 
%% @private
%% @doc
%% Adds info about itself and finally sends it to the target sever
%% @end
%% --------------------------------------------------------------------- 
handle_cast({connect, Target}, S=#state{name=Server, pid=Pid}) ->
    gen_server:cast({global, Target}, {connect, Server, Pid}),
    {noreply, S};

%% ---------------------------------------------------------------------  
%% @private
%% @doc
%% The callback activated in the server that another server tries to 
%% conenct to. It forwards the request to the leader if this server
%% is not. 
%%
%% Coordinates the insertion of the new server into the tables
%% across the cluster and passes the current state of the cluster 
%% to the new server.
%% @end
%% --------------------------------------------------------------------- 
handle_cast({connect, New, Pid}, S=#state{name=Server, 
                                                    leader=Leader, 
                                                    map=STbl,
                                                    channels=ChTbl}) ->
    case Leader == Server  of
        true -> 
            %% Tell other servers to insert new server into their tables
            %% and do it here
            CurrentServers = create_map(STbl),
            Recipients = lists:delete([Server], CurrentServers),
            lists:map(fun([T]) -> 
                       new(server, {New, Pid}, T) end, Recipients), 
            ets:insert(STbl, {New, Pid}),
            
            %% Take the tables representing the state of the whole
            %% cluster and pass them to the new server 
            PassMap = ets:tab2list(STbl),
            PassChannels = ets:tab2list(ChTbl),
            PassUsers = ets:tab2list(Server),
            Pass = {PassUsers, PassMap, PassChannels},
            gen_server:cast({global, New}, {init, Pass, Leader}),

            %% set up a monitor
            erlang:monitor(process, global:whereis_name(New)),
            {noreply, S};
        false -> gen_server:cast({global, Leader}, {connect, New, Pid}),
            {noreply, S}
    end;

%% --------------------------------------------------------------------- 
%% @private
%% @doc
%% After the cluster tells other servers about the new one it contacts
%% the new one through here and passes on the info about the whole
%% cluster in the New tuple. This function saves the info on the current
%% server. And sets up monitors of other servers.
%% @end
%% --------------------------------------------------------------------- 
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
    ServerList = create_map(STbl),
    lists:map(fun([Serv]) -> 
                erlang:monitor(process, global:whereis_name(Serv)) end,
              ServerList),
    {noreply, S#state{leader=Leader}};

%% --------------------------------------------------------------------- 
%% @private
%% @doc
%% Informs a non-leader server about a new server in the cluster, 
%% inserts the new server into the table and sets up a monitor of it.
%% @end
%% --------------------------------------------------------------------- 
handle_cast({server, {New, Pid}}, S=#state{map=STbl}) ->
    ets:insert(STbl, {New, Pid}),
    erlang:monitor(process, global:whereis_name(New)),
    {noreply, S};



%% ---------------------------------------------------------------------
%% @private
%% @doc
%% Forward the request to create a Channel to the leader
%% @end
%% --------------------------------------------------------------------- 
handle_cast({chanserv, Channel}, S=#state{leader=Leader}) ->
    gen_server:cast({global, Leader}, {create, Channel}),
    {noreply, S};

%% --------------------------------------------------------------------- 
%% @private
%% @doc
%% Insert a new channel into the leader's channel table and forward
%% the request to do the same to other servers.
%% Do nothing if the channel already exists.
%% @end
%% ---------------------------------------------------------------------  
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

%% ---------------------------------------------------------------------  
%% @private
%% @doc
%% Receives the forwarded request to add a new channel to the list.
%% Does that.
%% @end
%% --------------------------------------------------------------------- 
handle_cast({channel, Channel}, S=#state{channels=ChTbl}) ->
    ets:insert(ChTbl, {Channel, []}),
    {noreply, S};

%% --------------------------------------------------------------------- 
%% @private
%% @doc
%% Update channel's userlist and tell other servers to do the same
%% @end
%% --------------------------------------------------------------------- 
handle_cast({join, Name, Channel}, S=#state{name=Server, 
                                            map=STbl,
                                            channels=ChTbl}) ->
    %% create a list of server to forward the info about join to
    ServerList = create_map(STbl),
    Forward = lists:delete([Server], ServerList),

    %% update a channel's user list
    CurrentUsers = ets:lookup_element(ChTbl, Channel, 2),
    NewUsers = lists:append(CurrentUsers, [Name]),
    ets:update_element(ChTbl, Channel, {2, NewUsers}),

    %% tell other servers to do the same
    lists:map(fun([Serv]) ->
                new(new_join, {Name, Channel}, Serv) end, Forward),
    
    %% send a message to the channel so that other users see a
    %% message about the new user.
    gen_server:cast({global, Server},
                    {send_ch, Name, Channel, " *** joined the channel ***"}),
    {noreply, S};

%% ---------------------------------------------------------------------  
%% @private
%% @doc
%% Handles forwarded request to add a new user to an existing channel.
%% @end
%% ---------------------------------------------------------------------   
handle_cast({new_join, {Name, Channel}}, S=#state{channels=ChTbl}) ->
    CurrentUsers = ets:lookup_element(ChTbl, Channel, 2),
    NewUsers = lists:append(CurrentUsers, [Name]),
    ets:update_element(ChTbl, Channel, {2, NewUsers}),
    {noreply, S};

%% --------------------------------------------------------------------- 
%% @private
%% @doc
%% Print the message about user leaving to the channel, delete the user
%% from the channel and tell other servers to do the same.
%% @end
%% --------------------------------------------------------------------- 
handle_cast({leave, Name, Channel}, S=#state{name=Server,
                                             map=STbl,
                                             channels=ChTbl}) ->
    %% print a message to the channel about user leaving
    gen_server:cast({global, Server},
                    {send_ch, Name, Channel, "*** has left the channel ***"}),

    %% update the channel userlist
    CurrentUsers = ets:lookup_element(ChTbl, Channel, 2),
    NewUsers = lists:delete(Name, CurrentUsers),
    ets:update_element(ChTbl, Channel, {2, NewUsers}),

    %% forward the message about user leaving to other servers
    ServerList = create_map(STbl),
    Forward = lists:delete([Server], ServerList),
    lists:map(fun([Serv]) ->
                new(new_leave, {Name, Channel}, Serv) end, Forward),
    {noreply, S};

%% --------------------------------------------------------------------- 
%% @private
%% @doc
%% handle the forwarded message about user leaving
%% @end
%% --------------------------------------------------------------------- 
handle_cast({new_leave, {Name, Channel}}, S=#state{channels=ChTbl}) ->
    CurrentUsers = ets:lookup_element(ChTbl, Channel, 2),
    NewUsers = lists:delete(Name, CurrentUsers),
    ets:update_element(ChTbl, Channel, {2, NewUsers}),
    {noreply, S};



%% ---------------------------------------------------------------------  
%% @private
%% @doc
%% Callback for chat_client:send_channel/1,2 gets the list of users 
%% in the channel and sends them all messages thourgh the current
%% server. (Leaves finding out the host to send_msg).
%% @end
%% --------------------------------------------------------------------- 
handle_cast({send_ch, From, Ch, Msg}, S=#state{name=Server, 
                                               channels=ChTbl}) ->
    ChannelUsers = ets:lookup_element(ChTbl, Ch, 2),
    lists:map(fun(U) -> 
                send_msg(Server, U, {ch, From, Ch, Msg}) end, ChannelUsers),
    {noreply, S};

%% --------------------------------------------------------------------- 
%% @private
%% @doc
%% Receives a message about the user who has gone down, finds the entry
%% in the table and removes him.
%% @end
%% --------------------------------------------------------------------- 
handle_cast({quit, Pid}, S=#state{name=Server}) ->
    case ets:match(Server, {'$1', Pid, '_'}) of
        [[Nick]] -> 
            ets:delete(Server, Nick);
        _ -> ok
    end,
    {noreply, S};


%% --------------------------------------------------------------------- 
%% @private
%% @doc
%% Looks up the host of the addressee and forwards the message to it
%% if the Server is not host itself. (This ir redundant in case of
%% private messages because, they are already sent to host).
%% @end
%% --------------------------------------------------------------------- 
handle_cast({msg, To, Message}, S=#state{name=Server}) ->
    [[Pid, Host]] = ets:match(Server, {To, '$1', '$2'}),
    case Host of 
        Server ->
            Pid ! {msg, Message};
        Other ->
            send_msg(Other, To, Message)
    end,
    {noreply, S};

handle_cast(stop, S) ->
    {stop, normal, S};

handle_cast(_Msg, S) ->
    {noreply, S}.

%% --------------------------------------------------------------------- 
%% @private
%% @doc
%% Checks whether it was a server or client that has gone down. Handles
%% them down accordingly.
%% @end
%% --------------------------------------------------------------------- 
handle_info({'DOWN', _MRef, process, Pid, _}, S=#state{map=STbl}) ->
    ServerPids = ets:match(STbl, {'_', '$1'}),
    case lists:member([Pid], ServerPids) of
        true ->   %% it's a server
            NewState = handledown_server(Pid, S);
        false ->  %% it's a user
            %% create a list of servers and forward them the info
            ServerList = create_map(STbl),
            lists:map(fun([Serv]) -> 
                gen_server:cast({global, Serv}, {quit, Pid}) end, ServerList),
            NewState = S
    end,
    {noreply, NewState};

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
new(Tag, Message, Target) ->
    gen_server:cast({global, Target}, {Tag, Message}).

%% --------------------------------------------------------------------- 
%% @private
%% @doc
%% Unifies sending messages to users/channels. User handles the message
%% based on the tag of the Message tuple.
%% @end
%% --------------------------------------------------------------------- 
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
    ets:match(TableName, {'$1', '_'}).

%% --------------------------------------------------------------------- 
%% @private
%% @doc
%% Deletes server from the server list, sends itself a message about
%% every user that was affected, sets a new leader if needed.
%% @end
%% ---------------------------------------------------------------------  
handledown_server(DownPid, S=#state{name=Server, leader=Leader, map=STbl}) ->
    Network = ets:tab2list(STbl),
    {DownServer, _} = lists:keyfind(DownPid, 2, Network),
    ets:delete(STbl, DownServer),
    AffectedUsers = ets:match(Server, {'_', '$1', DownServer}),
    Map = create_map(STbl),
    lists:map(fun([Pid]) -> 
            gen_server:cast({global, Server}, {quit, Pid}) end, AffectedUsers),
    case DownServer == Leader of
        true -> SortedMap = lists:sort(Map),
            [[NewLeader] | _ ] = SortedMap;
        false -> NewLeader = Leader
    end,
    S#state{leader=NewLeader}.
