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
    Reply = jsonp(Callback, jsx:to_json(RawReply)),

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

jsonp(undefined, Reply) -> Reply;
jsonp(Callback, Reply) ->
    [Callback, <<"(">>, Reply, <<");">>].

reply(Key, <<"get">>, Proplist) ->
    Range = pval(<<"range">>, Proplist),
    [Start, End] = binary:split(Range, <<",">>), 

    case yasa:get(Key, Start, End) of
        {error, Reason} ->
            {500, [{<<"error">>, ?l2b(Reason)}]};
        Values ->
            {200, Values}
    end;

reply(Key, <<"set">>, Proplist) ->
    Timestamp = pval(<<"timestamp">>, Proplist),
    Value = pval(<<"value">>, Proplist),

    case yasa:set(Key, Value, Timestamp) of
        {error, Reason} ->
            {500, [{<<"error">>, ?l2b(Reason)}]};
        Reply ->
            {200, Reply}
    end;

reply(Key, <<"incr">>, Proplist) ->
    Timestamp = pval(<<"timestamp">>, Proplist),
    Value = pval(<<"value">>, Proplist),

    case yasa:incr(Key, Value, Timestamp) of
        {error, Reason} ->
            {500, [{<<"error">>, ?l2b(Reason)}]};
        Reply ->
            {200, Reply}
    end;

reply(_, _, _) ->
    {500, [{<<"error">>, <<"invalid request">>}]}.

pval(X, Req) when element(1, Req) == http_req ->
    {Val, _} = cowboy_http_req:qs_val(X, Req),
    Val;

pval(X, PL) ->
    proplists:get_value(X, PL).
