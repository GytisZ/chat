-module(chat_client).
-behaviour(gen_server).

%% API
-export([start/0, start/1, sign_in/2, sign_in/3, send/2, send/3, list_names/0,
         list_names/1, create/1, create/2, list_channels/0, list_channels/1,
         sign_out/0, sign_out/1, shutdown/0, shutdown/1, join/1, join/2, 
         list_ch_users/1, list_ch_users/2, leave/1, leave/2, 
         send_channel/2, send_channel/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% State
-record(state, {name, pid, server}).

%% ===================================================================== 
%% API
%% ===================================================================== 

%% ---------------------------------------------------------------------   
%% @doc
%% Starts the chat client. Takes one atom as an argument and uses it 
%% as a local name.
%% @end
%% ---------------------------------------------------------------------  
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start(RefName) ->
    gen_server:start_link({local, RefName}, ?MODULE, [], []).

%% ---------------------------------------------------------------------     
%% @doc
%% Signs the user in. ServerName is only used to establish a connection
%% with the server, user might end up being assigned to a different 
%% server.
%% @end
%% --------------------------------------------------------------------- 
sign_in(ServerName, Nick) ->
    gen_server:call(?MODULE, {sign_in, ServerName, Nick}).
sign_in(RefName, ServerName, Nick) ->
    gen_server:call(RefName, {sign_in, ServerName, Nick}).

%% --------------------------------------------------------------------- 
%% @doc 
%% Send a message to another user.
%% @end
%% --------------------------------------------------------------------- 
send(To, Message) ->
    gen_server:call(?MODULE, {sendmsg, To, Message}).
send(RefName, To, Message) ->
    gen_server:call(RefName, {sendmsg, To, Message}).

%% --------------------------------------------------------------------- 
%% @doc
%% List the names of all the users currently connected to the cluster.
%% @end
%% --------------------------------------------------------------------- 
list_names() ->
    gen_server:call(?MODULE, list_names).
list_names(RefName) ->
    gen_server:call(RefName, list_names).

%% --------------------------------------------------------------------- 
%% @doc
%% List the available channels
%% @end
%% --------------------------------------------------------------------- 
list_channels() ->
    gen_server:call(?MODULE, list_channels).
list_channels(RefName) ->
    gen_server:call(RefName, list_channels).

%% --------------------------------------------------------------------- 
%% @doc
%% Create a new channel
%% @end
%% --------------------------------------------------------------------- 
create(Channel) ->
    gen_server:call(?MODULE, {create, Channel}).
create(RefName, Channel) ->
    gen_server:call(RefName, {create, Channel}).

%% --------------------------------------------------------------------- 
%% @doc
%% Join a channel
%% @end
%% --------------------------------------------------------------------- 
join(Channel) ->
    gen_server:call(?MODULE, {join, Channel}).
join(RefName, Channel) ->
    gen_server:call(RefName, {join, Channel}).

%% --------------------------------------------------------------------- 
%% @doc
%% List users on a particular channel
%% @end
%% --------------------------------------------------------------------- 
list_ch_users(Channel) ->
    gen_server:call(?MODULE, {list_ch_users, Channel}).
list_ch_users(RefName, Channel) ->
    gen_server:call(RefName, {list_ch_users, Channel}).

%% --------------------------------------------------------------------- 
%% @doc
%% Leave a channel
%% @end
%% --------------------------------------------------------------------- 
leave(Channel) ->
    gen_server:call(?MODULE, {leave, Channel}).
leave(RefName, Channel) ->
    gen_server:call(RefName, {leave, Channel}).

%% --------------------------------------------------------------------- 
%% @doc
%% Send a message to the channel
%% @end
%% --------------------------------------------------------------------- 
send_channel(Channel, Message) ->
    gen_server:call(?MODULE, {send_ch, Channel, Message}).
send_channel(RefName, Channel, Message) ->
    gen_server:call(RefName, {send_ch, Channel, Message}).

sign_out() ->
    gen_server:cast(?MODULE, sign_out).
sign_out(RefName) ->
    gen_server:cast(RefName, sign_out).

shutdown() ->
    gen_server:call(?MODULE, stop).
shutdown(RefName) ->
    gen_server:call(RefName, stop).

%% ===================================================================== 
%% gen_server callbacks 
%% ===================================================================== 

init([]) ->
    {ok, #state{pid=self()}}.


%% --------------------------------------------------------------------- 
%% @private
%% @doc
%% Send the request to connect to a server. The server will reply with
%% a name of the server the client was assigned to, or a reason why the
%% client could not be connected.
%% @end
%% --------------------------------------------------------------------- 
handle_call({sign_in, ConnectServer, Name}, _From, S=#state{pid=Pid}) ->
    case gen_server:call({global, ConnectServer}, {nickserv, Name, Pid}) of
        {server, Server} ->
            io:format("You've been assigned to server ~p.~n", [Server]),
            erlang:monitor(process, global:whereis_name(Server)),
            {reply, ok, S#state{server=Server, name=Name}};
        already_signed_in ->
            io:format("You are already signed in.~n", []),
            {reply, ok, S};
        name_taken ->
            io:format("~p is taken. Select a different nick.~n", [Name]),
            {reply, name_taken, S}
    end;

handle_call({sendmsg, To, Msg}, _From, S=#state{server=Server,
                                                pid=Pid}) ->
    io:format("~p <<< ~p~n", [To, Msg]),
    gen_server:call({global, Server}, {sendmsg, Pid, To, Msg}),
    {reply, ok, S};

handle_call(list_names, _From, S=#state{server=Server}) ->
    {reply, chat_server:list_names(Server), S};

handle_call(list_channels, _From, S=#state{server=Server}) ->
    {reply, chat_server:list_channels(Server), S};

handle_call({create, Channel}, _From, S=#state{server=Server}) ->
    gen_server:cast({global, Server}, {chanserv, Channel}),
    {reply, ok, S};

handle_call({join, Channel}, _From, S=#state{server=Server, name=Name}) ->
    gen_server:cast({global, Server}, {join, Name, Channel}),
    {reply, ok, S};

handle_call({leave, Channel}, _From, S=#state{server=Server, name=Name}) ->
    gen_server:cast({global, Server}, {leave, Name, Channel}),
    {reply, ok, S};

handle_call({list_ch_users, Channel}, _From, S=#state{server=Server}) ->
    {reply, gen_server:call({global, Server}, {list_ch_users, Channel}), S};

handle_call({send_ch, Channel, Message}, _From, S=#state{server=Server,
                                                         name=Name}) ->
    gen_server:cast({global, Server}, {send_ch, Name, Channel, Message}),
    {reply, ok, S};

handle_call(stop, _From, S) ->
    {stop, normal, ok, S};

handle_call(_Request, _From, S) ->
    {noreply, S}.


handle_cast(sign_out, S=#state{server=Server, name=Nick}) ->
    gen_server:cast({global, Server}, {sign_out, Nick}),
    {noreply, S};

handle_cast({not_found, To}, S) ->
    io:format("~p - no such user.", [To]),
    {noreply, S};

handle_cast(_Message, S) ->
    {noreply, S}.

handle_info({msg, {priv, From, Message}}, S) ->
    io:format("~p >>> ~p~n", [From, Message]),
    {noreply, S};

handle_info({msg, {ch, Name, Ch, Message}}, S) ->
    io:format("#~p[~p]: ~p~n", [Ch, Name, Message]),
    {noreply, S};

handle_info({'DOWN', _, process, _, _}, S=#state{server=Server}) ->
    io:format("The server ~p has gone offline.~n", [Server]),
    io:format("Please connect to a server to continue chatting.~n"),
    {noreply, S#state{server=null, name=null}};

handle_info(_Info, S) ->
    {noreply, S}.

terminate(_Reason, _S) ->
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.
