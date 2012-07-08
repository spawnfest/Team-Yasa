-module(yasa_rra_queue_tests).
-include("rra_queue.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
rra_queue_test_() ->
    [{foreach, fun create_rra_queues/0, 
    	[fun test_gauge_push/1, 
    	 fun test_counter_push/1, 
    	 fun test_counter_push_prev/1]},
    {setup, fun create_long_rra_queue/0, fun test_select_range/1}].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%

create_rra_queues() ->
    [yasa_rra_queue:new(1, 1, 3), yasa_rra_queue:new(3, 3, 3)].

create_long_rra_queue() ->
    ?debugMsg("Generating RRA Queue..."),
	lists:foldl(fun(Elem, Acc) -> 
			timer:sleep(1000),
			yasa_rra_queue:push_gauge(Elem, Acc) 
	end, yasa_rra_queue:new(1, 10, 15), lists:seq(1, 15)). 

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

test_gauge_push([H | _Tail]) ->
    RQ = yasa_rra_queue:push_gauge(4, H),
    RQ1 = yasa_rra_queue:push_gauge(23, RQ),
	RQ2 = yasa_rra_queue:push_gauge(22, RQ1),
	RQ3 = yasa_rra_queue:push_gauge(1, RQ2),
	RQ4 = yasa_rra_queue:push_gauge(5, RQ3),
	RQ5 = yasa_rra_queue:push_gauge(6, RQ4),
    [?_assertEqual(RQ#rra_queue.step, H#rra_queue.step),
    ?_assertEqual(RQ#rra_queue.size, H#rra_queue.size),
    ?_assertEqual(RQ#rra_queue.ratio, H#rra_queue.ratio),
    ?_assertEqual(RQ5#rra_queue.step, H#rra_queue.step),
    ?_assertEqual(RQ5#rra_queue.size, H#rra_queue.size),
    ?_assertEqual(RQ5#rra_queue.ratio, H#rra_queue.ratio),
    ?_assertMatch({[{_, 4}], []}, RQ#rra_queue.queue),
    ?_assertMatch({[{_, 23}], [{_, 4}]}, RQ1#rra_queue.queue),
    ?_assertMatch({[{_, 22}, {_, 23}], [{_, 4}]}, RQ2#rra_queue.queue),
    ?_assertMatch({[{_, 1}, {_, 22}], [{_, 23}]}, RQ3#rra_queue.queue),
    ?_assertMatch({[{_, 5}, {_, 1}], [{_, 22}]}, RQ4#rra_queue.queue),
    ?_assertMatch({[{_, 6}, {_, 5}], [{_, 1}]}, RQ5#rra_queue.queue)].
	

test_counter_push([H, _S]) ->
	RQ = yasa_rra_queue:push_counter(4, H, undefined),
    RQ1 = yasa_rra_queue:push_counter(23, RQ, undefined),
	RQ2 = yasa_rra_queue:push_counter(22, RQ1, undefined),
	RQ3 = yasa_rra_queue:push_counter(1, RQ2, undefined),
	RQ4 = yasa_rra_queue:push_counter(5, RQ3, undefined),
	RQ5 = yasa_rra_queue:push_counter(6, RQ4, undefined),
    [?_assertEqual(RQ#rra_queue.step, H#rra_queue.step),
    ?_assertEqual(RQ#rra_queue.size, H#rra_queue.size),
    ?_assertEqual(RQ#rra_queue.ratio, H#rra_queue.ratio),
    ?_assertEqual(RQ5#rra_queue.step, H#rra_queue.step),
    ?_assertEqual(RQ5#rra_queue.size, H#rra_queue.size),
    ?_assertEqual(RQ5#rra_queue.ratio, H#rra_queue.ratio),
    ?_assertMatch({[{_, 4}], []}, RQ#rra_queue.queue),
    ?_assertMatch({[{_, 23}], [{_, 4}]}, RQ1#rra_queue.queue),
    ?_assertMatch({[{_, 22}, {_, 23}], [{_, 4}]}, RQ2#rra_queue.queue),
    ?_assertMatch({[{_, 1}, {_, 22}], [{_, 23}]}, RQ3#rra_queue.queue),
    ?_assertMatch({[{_, 5}, {_, 1}], [{_, 22}]}, RQ4#rra_queue.queue),
    ?_assertMatch({[{_, 6}, {_, 5}], [{_, 1}]}, RQ5#rra_queue.queue)].

test_counter_push_prev([H,S]) ->
	RQ = yasa_rra_queue:push_counter(4, H, undefined),
    RQ1 = yasa_rra_queue:push_counter(23, RQ, undefined),
	RQ2 = yasa_rra_queue:push_counter(22, RQ1, undefined),
	SRQ = yasa_rra_queue:push_counter(22, S, RQ2),
	[?_assertEqual(SRQ#rra_queue.step, S#rra_queue.step),
    ?_assertEqual(SRQ#rra_queue.size, S#rra_queue.size),
    ?_assertEqual(SRQ#rra_queue.ratio, S#rra_queue.ratio),
    ?_assertMatch({[{_, 49}], []}, SRQ#rra_queue.queue)].

test_select_range(QR) ->
	{value, {StartTime, _}} = queue:peek(QR#rra_queue.queue), 
	L1 = yasa_rra_queue:select_range(QR, StartTime, StartTime + 4),
	L2 = yasa_rra_queue:select_range(QR, StartTime - 1, StartTime + 50),
	L3 = yasa_rra_queue:select_range(QR, StartTime + 5, StartTime + 7),
	L4 = yasa_rra_queue:select_range(QR, StartTime - 1, StartTime + 3),
	L5 = yasa_rra_queue:select_range(QR, StartTime + 13, StartTime + 50),
	L6 = yasa_rra_queue:select_range(QR, StartTime + 17, StartTime + 50),
	R1 = [{StartTime + X -1 , X} || X <- lists:seq(1, 5)],
	R2 = [{StartTime + X -1 , X} || X <- lists:seq(1, 15)],
	R3 = [{StartTime + X -1 , X} || X <- lists:seq(6, 8)],
	R4 = [{StartTime + X -1 , X} || X <- lists:seq(1, 4)],
	R5 = [{StartTime + X -1 , X} || X <- lists:seq(14, 15)],
	[?_assertEqual(R1, L1),
	?_assertEqual(R2, L2),
	?_assertEqual(R3, L3),
	?_assertEqual(R4, L4),
	?_assertEqual(R5, L5),
	?_assertEqual([], L6)].


