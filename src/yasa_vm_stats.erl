%%%-------------------------------------------------------------------
%%% @doc
%%% Periodically dumps Erlang VM statistics. This is used for demo
%%% purposes
%%% @end
%%%-------------------------------------------------------------------
-module(yasa_vm_stats).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).
-define(REPORT_TIME, 1000).
-define(PVAL(X, PL), proplists:get_value(X, PL)).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    self() ! report,
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(report, State) ->
    schedule_report(?REPORT_TIME),

    MemoryUsage = erlang:memory(),

    yasa:set(<<"stats.total">>, ?PVAL(total, MemoryUsage)),
    yasa:set(<<"stats.memory_usage">>, ?PVAL(processes, MemoryUsage)),
    yasa:set(<<"stats.processes_used">>, ?PVAL(processes_used, MemoryUsage)),
    yasa:set(<<"stats.system">>, ?PVAL(system, MemoryUsage)),
    yasa:set(<<"stats.atom">>, ?PVAL(atom, MemoryUsage)),
    yasa:set(<<"stats.atom_used">>, ?PVAL(atom_used, MemoryUsage)),
    yasa:set(<<"stats.binary">>, ?PVAL(binary, MemoryUsage)),
    yasa:set(<<"stats.code">>, ?PVAL(code, MemoryUsage)),
    yasa:set(<<"stats.ets">>, ?PVAL(ets, MemoryUsage)),

    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

schedule_report(Time) ->
    erlang:send_after(Time, self(), report).
