-module(yasa_rra_queue).
-include("rra_queue.hrl").
-export([new/3, push_gauge/2, push_counter/2]).

new(Ratio, Step, Size) ->
    #rra_queue{
        queue = queue:new(),
        step = Step,
        size = Size,
        ratio = Ratio,
        previous_queue = undefined
    }.

push_gauge(Val, RRAQueue = #rra_queue{size = Size, queue = Queue}) ->
    Queue2 = queue:in({ts(), Val}, Queue),
    Queue3 = case queue:len(Queue2) > Size of
        true -> queue:out(Queue2);
        false -> Queue2
    end,
    RRAQueue#rra_queue{queue = Queue3}.

push_counter(Val, RRAQueue = #rra_queue{size = Size, 
    queue = Queue, ratio = Ratio, previous_queue = Prev}) ->
    Sum = sum_last_x(Prev, Ratio - 1) + Val,
    Queue2 = queue:in({ts(), Sum}, Queue),
    Queue3 = case queue:len(Queue2) > Size of
        true -> queue:out(Queue2);
        false -> Queue2
    end,
    RRAQueue#rra_queue{queue = Queue3}.
    
sum_last_x(Queue, X) ->
    sum_last_x(Queue, X, 0).

sum_last_x(_Queue, 0, Sum) ->
    Sum;
sum_last_x(Queue, X, Sum) ->
    {{value, {_TS, Val}}, NewQueue} = queue:out_r(Queue),
    sum_last_x(NewQueue, X - 1, Sum + Val).

ts() ->
    {Mega, Secs, _} = now(),
    Timestamp = Mega*1000000 + Secs.