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

-define(TIMEOUT, 60000).

-record(state, {primary, secondary, third, timer, type, value, clock_hit, retentions, ratios}).

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
    {ok, #state{}, ?TIMEOUT}.

handle_call({set, _Key, _TS, Value}, _From, State = #state{type = gauge}) ->
    {reply, ok, State#state{value = Value}};
%handle_call({incr, _Key, _TS, Value}, _From, State = #state{type = counter, value = OldValue}) ->
%    {reply, ok, State#state{value = OldValue + Value}};
handle_call({get, _Key, Start, End}, _From, State = #state{primary = Primary, 
    secondary = Secondary, third = Third}) ->
    WhichQueue = select_queue(Start, [Primary, Secondary, Third]),
    Reply = select_range_from_queue(WhichQueue, Start, End),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({init, [Key, Type]}, State) ->
    [Schema, ActualKey] = binary:split(Key, <<"\\.">> ),
    Path = get_path(Schema, ActualKey),
    [Type, Retentions, Primary, Secondary, Third] = 
        yasa_rra_file:load_from_file(Path, Type, Schema),
    Ratios = get_retention_ratios(Retentions),
    Tref = set_timer(Primary),
    {noreply, State#state{primary = Primary, secondary = Secondary, 
        third = Third, timer = Tref, type = Type, value = 0, clock_hit = 0, 
        ratios = Ratios, retentions = Retentions}};   
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({clock, _Step}, State = #state{primary = Primary, secondary = Secondary,
    third = Third, type = Type, value = Value, ratios = [SecRatio, ThirdRatio], 
    clock_hit = CH}) ->
    ok = push_into_queue(Value, Primary, CH , 1),
    ok = push_into_queue(Value, Secondary, CH , SecRatio),
    ok = push_into_queue(Value, Third, CH , ThirdRatio),
    Tref = set_timer(Primary),
    {noreply, State#state{timer = Tref, clock_hit = CH + 1}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State = #state{timer = Tref, retentions = Retantions, 
    primary = Primary, secondary = Secondary, third= Third, type = Type}) ->
    _Time = erlang:cancel_timer(Tref),
    ok = yasa_rra_file:save_to_file(Type, Retantions, Primary, Secondary, Third),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_path(Schema, Key) ->
    PrivDir = code:priv_dir(yasa),
    [PrivDir, "/", Schema, "/", Key].

set_timer({Step, _Size}) ->
    erlang:start_timer(Step * 1000, self(), {clock, Step}).

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