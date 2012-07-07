%% Module responsible for loading the data from yasa-rra file, if any exists,
%% into memory for the given key, or create a new one for new keys.
%% Handles all get/set/incr operation on the key 
%% it is responsible of. Periodicly saves the data in memory to a yasa file.
%% Save interval is randomly chosen betwenn 9 and 12 seconds.
%% Set operation is only allowed on gauges and increment in only allowed on
%% counters.


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
    path = [] :: list() | string(),     % path to yasa file for the key
    type = undefined :: undefined | gauge | counter,    % type for this key
                                                        % only gauges and counters for now
                                                        % timers will come soon!
    value = 0 :: integer(),         % current value
    counter = 1 :: integer(),       % counter to keep track of clock hits
    retentions = [] :: [{integer(), integer()}],
    rra_queues = [] :: [#rra_queue{}]   % rra queues which holds actaul data 
                                        % along with some meta data
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Key, Type) ->
    gen_server:start_link(?MODULE, [Key, Type], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Key, Type]) ->
    gen_server:cast(self(), {init, [Key, Type]}), 
    {ok, #state{}}.

%%handle calls to respond get/set/increment operations
handle_call({set, Value}, _From, State = #state{type = gauge}) ->
    {reply, ok, State#state{value = Value}};
handle_call({incr, Value}, _From, State = #state{type = counter, value = OldValue}) ->
    {reply, ok, State#state{value = Value + OldValue}};
handle_call({get, Start, End}, _From, State = #state{rra_queues = RQS}) ->
    WhichQueue = select_queue(Start, RQS),
    Reply = select_range_from_queue(WhichQueue, Start, End),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% process initialization
handle_cast({init, [SchemaDotKey, Req]}, State) ->
    Path = path_from_key(SchemaDotKey),
    self() ! write_to_disk,
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

%% handle clock ticks and disk writes
handle_info({tick, _Step}, State = #state{type = gauge, value = Value, rra_queues = RQS,
    counter = Counter}) ->
    _Tref = schedule_tick(RQS),
    NewRQS = insert_into_gauges(Value, Counter, RQS),
    {noreply, State#state{counter = Counter + 1, rra_queues = NewRQS}};
handle_info({tick, _Step}, State = #state{type = counter, value = Value, rra_queues = RQS,
    counter = Counter}) ->
    _Tref = schedule_tick(RQS),
    NewRQS = insert_into_counters(Value, Counter, RQS),
    {noreply, State#state{counter = Counter + 1, value = 0, rra_queues = NewRQS}};
handle_info(write_to_disk, State = #state{rra_queues = RQS, type = Type, 
    retentions = Rets,path = Path}) ->
    yasa_rra_file:save_to_file(Path, {Type, Rets, RQS}),
    After = crypto:rand_uniform(9000, 12000), 
    erlang:send_after(After, self(), write_to_disk),
    {noreply, State};
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
%% @private get the path to yasa file from key
path_from_key(Key) ->
    SlashedKey = binary:replace(Key, <<".">>, <<"/">>, [global]),
    PrivDir = yasa_app:priv_dir(),
    [PrivDir, "/storage/", binary_to_list(SlashedKey), ".yasa"].

%% @private check if wanted operation is allowed on this key
%% at initialization
check_type_integrity(gauge, set) -> ok;
check_type_integrity(counter, incr) -> ok;
check_type_integrity(_, get) -> ok;
check_type_integrity(_, _) -> throw("type mismatch").

%% @private determine type from operation if creating a new
%% key
type_from_req(set) -> gauge;
type_from_req(incr) -> counter;
type_from_req(get) -> throw("can't get a non-existing key").

%% get retentions from app config for new keys
get_retentions() ->
    case application:get_env(yasa, retentions) of 
        {ok, Rets} -> Rets
    end.

%% @private create new rra_queue from retention and
%% and ratios calculated from them.
zip_rra_queues(Retentions) ->
    Ratios = get_retention_ratios(Retentions), 
    Combine = fun(Ratio, {Step, Size}) ->
        yasa_rra_queue:new(Ratio, Step, Size)
    end,
    lists:zipwith(Combine, Ratios, Retentions).

%% @private schedule a tick from the step size of the first
%% retention
schedule_tick([Head | _Tail]) ->
    Step = Head#rra_queue.step,
    erlang:send_after(Step * 1000, self(), {tick, Step}).

%% @private calculate the ratios between the first retention
%% and the following ones
%% ex: 
%% if your retentions are 
%% [{10, 2160} ,{60, 10080}, {600, 262974}]
%% ratios will be [1,6,60]
get_retention_ratios([{Step, _Size} | _Tail] = Retentions) ->
    Fun = fun({Step1, _}) -> Step1 div Step end,
    lists:map(Fun, Retentions).

%% @private insert a new value into proper queues for gauges
insert_into_gauges(Value, Counter, RQS) ->
    Mapper = fun(RQ = #rra_queue{ratio = Ratio}) ->
        case Counter rem Ratio of
            0 -> yasa_rra_queue:push_gauge(Value, RQ);
            _ -> RQ
        end
    end,
    lists:map(Mapper, RQS).

%% @private insert a new value into proper queues for counters
insert_into_counters(Value, Counter, RQS) ->
    Accumulate = fun(RQ = #rra_queue{ratio = Ratio}, Acc) ->
        case Counter rem Ratio of
            0 ->
                Prev = case Acc of
                    [] -> undefined;
                    _ -> hd(Acc)
                end, 
                [yasa_rra_queue:push_counter(Value, RQ, Prev) | Acc];
            _ -> [RQ | Acc]
        end
    end,
    lists:reverse(lists:foldl(Accumulate, [], RQS)).

%% @private select a proper queue to query for the the given 
%% start time. If none of the queues cover the given start time
%% return the queue for first retention since it has the most
%% data points in it.
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

%%%% @private extract given range from the queue and return it as a
%%% list
select_range_from_queue(RQ, Start, End) ->
    yasa_rra_queue:select_range(RQ, Start, End).
