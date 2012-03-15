-module(chat_server).

-behaviour(gen_server).

%% sort of public
-export([start_link/0, sign_in/2, sign_out/2, list_names/1]).

%% not so public
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Server state
-record(state, {name_list=gb_sets:new()}).


%%% PUBLIC API

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% Try signing in. Server may refuse if the user name is taken
sign_in(ServerRef, Nick) ->
    gen_server:call(ServerRef, {sign_in, Nick}).

%% Sign out. This is asynchronous till client will be separated from the server
sign_out(ServerRef, Nick) ->
    gen_server:cast(ServerRef, {sign_out, Nick}).

%% Get the list of all the users currently connected to the server
list_names(ServerRef) ->
    gen_server:call(ServerRef, list_names).



%%% Server functions

init([]) ->
    {ok, #state{name_list=gb_sets:empty()}}.


handle_call({sign_in, Nick}, _From, State = #state{name_list=List}) ->
    case gb_sets:is_element(Nick, List) of
        false -> {reply, ok, State#state{name_list=gb_sets:add(Nick, List)}};
        true  -> {reply, name_taken, State}
    end;
handle_call(list_names, _From, State=#state{name_list=List}) ->
    {reply, gb_sets:to_list(List), State}.


handle_cast({sign_out, Nick}, State=#state{name_list=List}) ->
    gb_sets:del_element(Nick, List),
    {noreply, State#state{name_list=List}}.


%% Placeholders for now
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(normal, _State) -> 
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
