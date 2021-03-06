-module(chat_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start/1, stop/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(SERVER(Name),
        {Name, {chat_server, start_link, [Name]},
         temporary,
         5000,
         worker,
         [chat_server]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start(Name) ->
    supervisor:start_child(?MODULE, ?SERVER(Name)).

stop(Name) ->
    supervisor:terminate_child(?MODULE, Name),
    supervisor:delete_child(?MODULE, Name).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.
