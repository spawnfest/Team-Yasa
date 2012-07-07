-module(yasa_rra).
-behaviour(gen_server).

-include("rra_queue.hrl").
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
    path = [] :: list() | string(),
    type = undefined :: undefined | gauge | counter,
    value = 0 :: integer(),
    counter = 1 :: integer(),
    retentions = [] :: [{integer(), integer()}],
    rra_queues = [] :: [#rra_queue{}]
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

handle_call({set, _TS, Value}, _From, State = #state{type = gauge}) ->
    {reply, ok, State#state{value = Value}};
handle_call({incr, _TS, Value}, _From, State = #state{type = counter, value = OldValue}) ->
    {reply, ok, State#state{value = Value + OldValue}};
handle_call({get, Start, End}, _From, State = #state{rra_queues = RQS}) ->
    WhichQueue = select_queue(Start, RQS),
    Reply = select_range_from_queue(WhichQueue, Start, End),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({init, [SchemaDotKey, Req]}, State) ->
    Path = path_from_key(SchemaDotKey),
    case yasa_rra_file:load_from_file(Path) of
        {Type, Retentions, RRAQueues} ->
            ok = check_type_integrity(Type, Req),
            _Tref = schedule_tick(RRAQueues),
            {noreply, State#state{type=Type, retentions = Retentions,
                rra_queues=RRAQueues, path = Path}};
        {error, not_found} ->
            Type = type_from_req(Req),
            Retentions = get_retentions(),
            RRAQueues = zip_rra_queues(Retentions),
            _Tref = schedule_tick(RRAQueues),
            {noreply, State#state{type=Type, retentions = Retentions,
                rra_queues=RRAQueues, path = Path}};
        {error, Reason} ->
            {stop, Reason}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tick, _Step}, State = #state{type = gauge, value = Value, rra_queues = RQS,
    counter = Counter, retentions = Rets, path = Path}) ->
    _Tref = schedule_tick(RQS),
    NewRQS = insert_into_gauges(Value, Counter, RQS),
    yasa_rra_file:save_to_file(Path, {gauge, Rets, NewRQS}),
    {noreply, State#state{counter = Counter + 1, rra_queues = NewRQS}};
handle_info({tick, _Step}, State = #state{type = counter, value = Value, rra_queues = RQS,
    counter = Counter, retentions = Rets,path = Path}) ->
    _Tref = schedule_tick(RQS),
    NewRQS = insert_into_counters(Value, Counter, RQS),
    yasa_rra_file:save_to_file(Path, {counter, Rets, NewRQS}),
    {noreply, State#state{counter = Counter + 1, value = 0, rra_queues = NewRQS}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State = #state{type = Type, retentions = Rets, 
    rra_queues = RQS, path = Path}) ->
    yasa_rra_file:save_to_file(Path, {Type, Rets, RQS}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
path_from_key(Key) ->
    PrivDir = yasa_app:priv_dir(),
    [PrivDir, "/", binary_to_list(Key), ".yasa"].

check_type_integrity(gauge, set) -> ok;
check_type_integrity(counter, incr) -> ok;
check_type_integrity(_, get) -> ok;
check_type_integrity(_, _) -> throw("type mismatch").

type_from_req(set) -> gauge;
type_from_req(incr) -> counter;
type_from_req(get) -> throw("can't get a non-existing key").

get_retentions() ->
    case application:get_env(yasa, retentions) of 
        {ok, Rets} -> Rets
    end.

zip_rra_queues(Retentions) ->
    Ratios = get_retention_ratios(Retentions), 
    Combine = fun(Ratio, {Step, Size}) ->
        yasa_rra_queue:new(Ratio, Step, Size)
    end,
    RRAQueues = lists:zipwith(Combine, Ratios, Retentions),
    set_previous_queues(RRAQueues, [], undefined).

set_previous_queues([Head | Tail], Acc, Previous) ->
    NewRRAQueue = Head#rra_queue{previous_queue = Previous},
    NewPrevious = Head#rra_queue.queue,
    set_previous_queues(Tail, [NewRRAQueue | Acc], NewPrevious);
set_previous_queues([], Acc, _) ->
    lists:reverse(Acc).

schedule_tick([Head | _Tail]) ->
    Step = Head#rra_queue.step,
    erlang:send_after(Step * 1000, self(), {tick, Step}).

get_retention_ratios([{Step, _Size} | _Tail] = Retentions) ->
    Fun = fun({Step1, _}) -> Step1 div Step end,
    lists:map(Fun, Retentions).

insert_into_gauges(Value, Counter, RQS) ->
    Mapper = fun(RQ = #rra_queue{ratio = Ratio}) ->
        case Counter rem Ratio of
            0 -> yasa_rra_queue:push_gauge(Value, RQ);
            _ -> RQ
        end
    end,
    lists:map(Mapper, RQS).

insert_into_counters(Value, Counter, RQS) ->
    Mapper = fun(RQ = #rra_queue{ratio = Ratio}) ->
        case Counter rem Ratio of
            0 -> yasa_rra_queue:push_counter(Value, RQ);
            _ -> RQ
        end
    end,
    lists:map(Mapper, RQS).

select_queue(Start, RQS) ->
    select_queue(Start, RQS, hd(RQS)).

select_queue(_Start, [], Default) ->
    Default;
select_queue(Start, [H = #rra_queue{queue = Queue}|Tail], Default) ->
    case queue:peek(Queue) of
        {value, {TS, _Val}} ->
            case TS < Start of
                true -> H; 
                false -> select_queue(Start, Tail, Default)
            end;
        empty ->
            select_queue(Start, Tail, Default)
    end.

%%%% TODO actually write these function   
select_range_from_queue(RQ, Start, End) ->
    yasa_rra_queue:select_range(RQ, Start, End).
