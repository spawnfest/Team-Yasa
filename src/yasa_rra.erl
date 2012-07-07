-module(yasa_rra).
-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
    type,
    value = 0,
    counter = 0,
    retentions
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Key, Type) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Key, Type], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Key, Type]) ->
    gen_server:cast(self(), {init, [Key, Type]}), 
    {ok, #state{}}.

handle_call({set, _Key, _TS, Value}, _From, State = #state{type = gauge}) ->
    {reply, ok, State#state{value = Value}};

handle_call({get, _Key, Start, End}, _From, State = #state{primary = Primary, 
    secondary = Secondary, third = Third}) ->
    WhichQueue = select_queue(Start, [Primary, Secondary, Third]),
    Reply = select_range_from_queue(WhichQueue, Start, End),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({init, [Key, Type]}, State) ->
    Path = path_from_key(Key),

    case yasa_rra_file:open(Path) of
        {SchemaType, Retentions} ->
            {noreply, #state{type=Type, retentions=Retentions}};
        {error, Reason} ->
            {stop, Reason} 
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tick, _Step}, State = #state{retentions=Retentions,
    type = Type, value = Value, ratios = [SecRatio, ThirdRatio], counter = Counter}) ->

    schedule_tick(Primary),

    ok = push_into_queue(Value, Retentions#retentions.primary, Counter, 1),
    ok = push_into_queue(Value, Retentions#retentions.secondary, Counter, SecRatio),
    ok = push_into_queue(Value, Retentions#retentions.third, Counter, ThirdRatio),

    {noreply, State#state{counter = Counter + 1}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

path_from_key(Key) ->
    [Schema, ActualKey] = binary:split(Key, <<"\\.">> ),
    PrivDir = code:priv_dir(yasa),
    [PrivDir, "/", Schema, "/", ActualKey].

schedule_tick({Step, _Size}) ->
    erlang:send_after(Step * 1000, self(), {tick, Step}).

get_retention_ratios([{Step, _Size} | Tail]) ->
    Fun = fun({Step1, _}) -> Step1 / Step end,
    lists:map(Fun, Tail).

push_into_queue(Value, Queue, Hit, Ratio) ->
    case Hit rem Ratio of
        0 -> insert();
        _ -> ok
    end.

insert() -> ok.
%%%% TODO actually write these function
select_queue(Start, [H |Tail]) ->
    H.
select_range_from_queue([H | Tail], Start, End) ->
    [H].
