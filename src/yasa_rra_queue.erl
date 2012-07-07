-module(yasa_rra_queue).
-include("rra_queue.hrl").
-export([new/3, push_gauge/2, push_counter/3, select_range/3]).

new(Ratio, Step, Size) ->
    #rra_queue{
        queue = queue:new(),
        step = Step,
        size = Size,
        ratio = Ratio
    }.

push_gauge(Val, RRAQueue = #rra_queue{size = Size, queue = Queue}) ->  
    Queue2 = queue:in({ts(), Val}, Queue),
    Queue3 = case queue:len(Queue2) > Size of
        true -> 
            {_, Q} = queue:out(Queue2),
            Q;
        false -> Queue2
    end,
    RRAQueue#rra_queue{queue = Queue3}.

push_counter(Val, RRAQueue = #rra_queue{size = Size, 
    queue = Queue, ratio = Ratio}, PrevRQ) ->
    Sum = case PrevRQ of
        undefined -> Val;
        _ -> 
            PrevQ = PrevRQ#rra_queue.queue,
            sum_last_x(PrevQ, Ratio)
    end,
    Queue2 = queue:in({ts(), Sum}, Queue),
    Queue3 = case queue:len(Queue2) > Size of
        true -> 
            {_, Q} = queue:out(Queue2),
            Q;
        false -> Queue2
    end,
    RRAQueue#rra_queue{queue = Queue3}.

select_range(#rra_queue{queue = Queue}, Start, End) ->
    Filter = fun({TS, _Val}) ->
        case {Start < TS, TS < End} of
            {true, true} -> true;
            _ -> false
        end
    end,
    queue:to_list(queue:filter(Filter, Queue)).
%%%===================================================================
%%% Internal functions
%%%===================================================================   
sum_last_x(Queue, X) ->
    sum_last_x(Queue, X, 0).

sum_last_x(_Queue, 0, Sum) ->
    Sum;
sum_last_x(Queue, X, Sum) ->
    io:format("~p    ~p     ~p~n", [Queue, X, Sum]), 
    {{value, {_TS, Val}}, NewQueue} = queue:out_r(Queue),
    sum_last_x(NewQueue, X - 1, Sum + Val).

ts() ->
    {Mega, Secs, _} = now(),
    Mega*1000000 + Secs.