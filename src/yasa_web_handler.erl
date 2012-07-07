-module(yasa_web_handler).

%http callbacks
-export([init/3, handle/2, terminate/2]).
%websocket callbacks
-export([websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3]).

-define(l2b(X), list_to_binary(X)).

init({tcp,http}, Req, _Opts) -> 
    {IsWebSocket,_} = cowboy_http_req:header('Upgrade',Req,false),
    case IsWebSocket of
        <<"websocket">> ->
            {upgrade, protocol, cowboy_http_websocket};
        false ->
            {ok, Req, undefined_state}
    end.

handle(Req, State) ->
    http_handle(Req, State).

websocket_init(_TransportName, Req, _Opts) ->
    {ok, Req, undefined_state}.

%
% HTTP Handler
%

http_handle(Req, State) ->
    ReplyHeader = [{'Content-Type', "application/json; charset=utf-8"}],

    {Method, _} = cowboy_http_req:binding(action, Req),
    Key = pval(<<"key">>, Req),
    Callback = pval(<<"callback">>, Req), % json-p callback

    {Status, RawReply} = reply(Key, Method, Req),
    Reply = jsonp(Callback, to_json(RawReply)),

    {ok, Req2} = cowboy_http_req:reply(Status, ReplyHeader, Reply, Req),
    {ok, Req2, State}.

%
% Websocket Handler
%

websocket_handle({text, Json}, Req, State) ->
    Proplist = jsx:to_term(Json),
    Method = proplists:get_value(<<"method">>, Proplist), 
    Key = pval(<<"key">>, Proplist), 

    RawReply = reply(Key, Method, Proplist),
    Reply = jsx:to_json(RawReply),

    {reply, {text, Reply}, Req, State};

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
terminate(_Req, _State) ->
    ok.

% ============================================================================
% Internal Functions
% ============================================================================

reply(Key, <<"get">>, Proplist) ->
    [Start, End] = handle_range(pval(<<"range">>, Proplist)),

    case yasa:get(Key, Start, End) of
        {error, Reason} ->
            {500, [{<<"error">>, ?l2b(Reason)}]};
        Values ->
            {200, lists:map(fun({T, V}) -> [T,V] end, Values)}
    end;

reply(Key, <<"set">>, Proplist) ->
    Value = pval(<<"value">>, Proplist),

    case yasa:set(Key, to_int(Value)) of
        {error, Reason} ->
            {500, [{<<"error">>, ?l2b(Reason)}]};
        Reply ->
            {200, Reply}
    end;

reply(Key, <<"incr">>, Proplist) ->
    Value = pval(<<"value">>, Proplist),

    case yasa:incr(Key, to_int(Value)) of
        {error, Reason} ->
            {500, [{<<"error">>, ?l2b(Reason)}]};
        Reply ->
            {200, Reply}
    end;

reply(_, _, _) ->
    {500, [{<<"error">>, <<"invalid request">>}]}.

jsonp(undefined, Reply) -> Reply;
jsonp(Callback, Reply) ->
    [Callback, <<"(">>, Reply, <<");">>].

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

to_json(ok) ->
    <<"[]">>;
to_json(Term) ->
    jsx:to_json(Term).

to_int(Bin) when is_binary(Bin) ->
    list_to_integer(binary_to_list(Bin)).

to_range(N, Period) ->
    [timestamp() - seconds_in(N, Period), timestamp()].

seconds_in(N, year)   -> N * 365 * 24 * 60 * 60;
seconds_in(N, month)  -> N * 31 * 24 * 60 * 60;
seconds_in(N, day)    -> N * 24 * 60 * 60;
seconds_in(N, hour)   -> N * 60 * 60;
seconds_in(N, min)    -> N * 60;
seconds_in(N, sec)    -> N.
    
parse_matches(String, {match, [_|T]}) ->
    parse_matches(String, T, []).
parse_matches(_, [], Acc) ->
    Acc;
parse_matches(String, [{Start, End} | Rest], Acc) ->
    <<_:Start/binary, Match:End/binary, _/binary>> = String,
    parse_matches(String, Rest, Acc ++ [Match]).

pval(X, Req) when element(1, Req) == http_req ->
    {Val, _} = cowboy_http_req:qs_val(X, Req),
    Val;
pval(X, PL) ->
    proplists:get_value(X, PL).

timestamp() ->
    {Mega, Secs, _} = now(),
    Mega*1000000 + Secs.
