-module(yasa_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    RestartStrategy = {one_for_one, 5, 10},
    Children = [
        ?CHILD(yasa_rra_sup, supervisor),
        ?CHILD(yasa_pid_store, worker),
        ?CHILD(yasa_vm_stats, worker)
    ],
    {ok, {RestartStrategy, Children}}.
