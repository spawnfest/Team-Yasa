-module(yasa_rra_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_child/2]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), 
        {Id, {Mod, start_link, Args}, permanent, 5000, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Key, Type) ->
    supervisor:start_child(?MODULE, [Key, Type]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 3,
    MaxSecondsBetweenRestarts = 10, 

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = transient, % restart child if it exits
                         % with something other than {'EXIT', normal}
    Shutdown = 2000,
    Type = worker,

    RRA = {'yasa_rra', {'yasa_rra', start_link, []},
                      Restart, Shutdown, Type, ['yasa_rra']},

    {ok, {SupFlags, [RRA]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
