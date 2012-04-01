-module(chat_client_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start/0, start/1, stop/0, stop/1]).

%% Supervisor callbacks
-export([init/1]).

-define(CLIENT(Name),
        {Name, {chat_client, start, [Name]},
         temporary,
         5000,
         worker,
         [chat_client]}).

%% ===================================================================== 
%% API
%% ===================================================================== 

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start() ->
    supervisor:start_child(?MODULE, {client,
                                     {chat_client, start, []},
                                     temporary,
                                     5000,
                                     worker,
                                     [chat_client]}).

start(Name) ->
    supervisor:start_child(?MODULE, ?CLIENT(Name)).

stop() ->
    chat_client_sup:stop(client).

stop(Name) ->
    supervisor:terminate_child(?MODULE, Name).

%% ===================================================================== 
%% Supervisor callbacks
%% ===================================================================== 

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.
