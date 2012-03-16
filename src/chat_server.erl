-module(chat_server).

-behaviour(gen_server).

%% sort of public
-export([start_link/0, sign_in/2, sign_out/1, list_names/0, shutdown/0,
        send_message/2]).

%% not so public
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Server state
-record(state, {name_list=dict:new()}).
-define(SERVER, ?MODULE).

%%%%%%%%%%%%%%%%%%%
%%% PUBLIC API %%%%
%%%%%%%%%%%%%%%%%%%


start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).


%% Try signing in. Server may refuse if the user name is taken
sign_in(Nick, Pid) ->
    gen_server:call({global, ?SERVER}, {sign_in, Nick, Pid}).


%% Sign out. This is asynchronous till client will be separated from the server
sign_out(Nick) ->
    gen_server:cast({global, ?SERVER}, {sign_out, Nick}).

%% Send message
send_message(Nick, Message) ->
    gen_server:call({global, ?SERVER}, {sendmsg, Nick, Message}).

%% Get the list of all the users currently connected to the server
list_names() ->
    gen_server:call({global, ?SERVER}, list_names).


%% Shut down the server
shutdown() ->
    gen_server:cast({global, ?SERVER}, stop).


%%% Server functions

init([]) ->
    {ok, #state{name_list=dict:new()}}.


handle_call({sign_in, Nick, Pid}, _From, State = #state{name_list=List}) ->
    case dict:is_key(Nick, List) of
        false ->
            {reply, ok, State#state{name_list=dict:append(Nick, Pid,  List)}};
        true  ->
            {reply, name_taken, State}
    end;

handle_call(list_names, _From, State=#state{name_list=List}) ->
    {reply, dict:fetch_keys(List), State};

handle_call({sendmsg, To, Message}, _From, State=#state{name_list=List}) ->
    case dict:is_key(To, List) of
        true ->
            [Pid] = dict:fetch(To, List),
            Pid ! {printmsg, Message};
        false -> ok
    end,
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({sign_out, Nick}, State=#state{name_list=List}) ->
    NewList= dict:erase(Nick, List),
    {noreply, State#state{name_list=NewList}}.


%% Placeholders for now
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(normal, _State) -> 
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
