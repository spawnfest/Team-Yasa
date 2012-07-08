-module(yasa_ws_handler).

-export([init/4, stream/3, info/3, terminate/2]).

-record(state, {
	registered = [] :: [binary()]
}).

init(_Transport, Req, _Opts, _Active) ->
	erlang:start_timer(10000, self(), tick),
    {ok, Req, #state{}}.

stream(<<"ping">>, Req, State) ->
	{reply, <<"pong">>, Req, State};
stream(Data, Req, State = #state{registered = Registered}) ->
	Json = jsx:to_term(Data),
	Type = proplists:get_value(<<"type">>, Json),
	Key = proplists:get_value(<<"key">>, Json),
	Range = proplists:get_value(<<"range">>, Json), 
	case Type of 
		<<"unregister">> ->
    		{reply, <<"ok">>, Req, State#state{registered = lists:delete(Key, Registered)}};
    	<<"register">> ->
    		Values = get(Key, Range),
    		Reply = [{key, Key}, {values, Values}],
    		{reply, jsx:to_json([Reply]), Req, State#state{registered = [Key | Registered]}}
    end.

info({timeout, _TRef, tick}, Req, State = #state{registered = Registered}) ->
	io:format("qewqwe~n"),
	erlang:start_timer(10000, self(), tick),	
	Mapper = fun(Key) ->
		Values = get(Key, <<"-1min">>),
		[{key, Key}, {values, Values}]
	end,
	Data = lists:map(Mapper, Registered),
	{reply, jsx:to_json(Data), Req, State };
info(_Info, Req, State) ->
	io:format("~p~n", [_Info]), 
    {ok, Req, State}.

terminate(_Req, _State) ->
    ok.


get(Key, Range) ->
    [Start, End] = handle_range(Range),
    Values = yasa:get(Key, Start, End),
    lists:map(fun({T, V}) -> [T,V] end, Values).

handle_range(String) when is_binary(String) ->
    Regex = "-(\\d+)(hour|min|sec|day|month|year)",
    [Size_, Period_] = case re:run(String, Regex) of
        nomatch ->
            {error, nomatch};
        Match ->
            parse_matches(String, Match)
    end,
    Size = list_to_integer(binary_to_list(Size_)),
    Period = list_to_atom(binary_to_list(Period_)),
    Reply = to_range(Size, Period),
    Reply.

parse_matches(String, {match, [_|T]}) ->
    parse_matches(String, T, []).
parse_matches(_, [], Acc) ->
    Acc;
parse_matches(String, [{Start, End} | Rest], Acc) ->
    <<_:Start/binary, Match:End/binary, _/binary>> = String,
    parse_matches(String, Rest, Acc ++ [Match]).

to_range(N, Period) ->
    [timestamp() - seconds_in(N, Period), timestamp()].

seconds_in(N, year)   -> N * 365 * 24 * 60 * 60;
seconds_in(N, month)  -> N * 31 * 24 * 60 * 60;
seconds_in(N, day)    -> N * 24 * 60 * 60;
seconds_in(N, hour)   -> N * 60 * 60;
seconds_in(N, min)    -> N * 60;
seconds_in(N, sec)    -> N.

timestamp() ->
    {Mega, Secs, _} = now(),
    Mega*1000000 + Secs.