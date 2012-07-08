-module(yasa_handler_utils).

-export([handle_range/1, reply/3, pval/2]).

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

reply(Key, <<"get">>, Proplist) ->
    [Start, End] = handle_range(pval(<<"range">>, Proplist)),
    Values = yasa:get(Key, Start, End),
    {200, lists:map(fun({T, V}) -> [T,V] end, Values)};
reply(Key, <<"set">>, Proplist) ->
    Value = pval(<<"value">>, Proplist),
    ok = yasa:set(Key, to_int(Value)),
    {200, <<"">>};
reply(Key, <<"incr">>, Proplist) ->
    Value = pval(<<"value">>, Proplist),
    ok = yasa:incr(Key, to_int(Value)),
    {200, <<"">>};
reply(undefined, <<"keys">>, _) ->
    {200, yasa:keys()};
reply(_, _, _) ->
    {500, <<"error:invalid request">>}.

pval(X, Req) when element(1, Req) == http_req ->
    {Val, _} = cowboy_http_req:qs_val(X, Req),
    Val;
pval(X, PL) ->
    proplists:get_value(X, PL).
    
% ============================================================================
% Internal Functions
% ============================================================================ 
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

to_int(Bin) when is_binary(Bin) ->
    list_to_integer(binary_to_list(Bin)).

timestamp() ->
    {Mega, Secs, _} = now(),
    Mega*1000000 + Secs.