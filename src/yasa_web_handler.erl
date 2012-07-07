-module(yasa_web_handler).

%http callbacks
-export([init/3, handle/2, terminate/2]).
%websocket callbacks
-export([websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3]).

init({tcp,http}, Req, _Opts) -> 
    {IsWebSocket,_} = cowboy_http_req:header('Upgrade',Req,false),
    case IsWebSocket of
        <<"websocket">> ->
            {upgrade, protocol, cowboy_http_websocket};
        false ->
            {ok, Req, undefined_state}
    end.

websocket_init(_TransportName, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    {Action, _} = cowboy_http_req:binding(action, Req),
    {Key, _} = cowboy_http_req:qs_val(<<"key">>, Req),
 
    RawReply = case Action of 
        <<"get">> ->
            {Range, _} = cowboy_http_req:qs_val(<<"range">>, Req),
            [Start, End] = binary:split(Range, <<",">> ), 
            yasa:get(Key, Start, End);
        <<"set">> ->
            {TS, _} = cowboy_http_req:qs_val(<<"timestamp">>, Req),
            {Value, _} = cowboy_http_req:qs_val(<<"value">>, Req),
            yasa:set(Key, Value, TS);
        <<"incr">> ->
            {TS, _} = cowboy_http_req:qs_val(<<"timestamp">>, Req),
            {Value, _} = cowboy_http_req:qs_val(<<"value">>, Req),
            yasa:incr(Key, Value, TS)
    end,
    Reply = process_raw_reply(RawReply),
    {ok, Req2} = cowboy_http_req:reply(200, [], Reply, Req),
    {ok, Req2, State}.

websocket_handle({text, Json}, Req, State) ->
    Op = jsx:to_term(Json),
    Method = proplists:get_value(<<"method">>, Op), 
    Key = proplists:get_value(<<"key">>, Op), 
    RawReply = case Method of 
        <<"get">> ->
            Range = proplists:get_value(<<"range">>, Op),
            [Start, End] = binary:split(Range, <<",">> ), 
            yasa:get(Key, Start, End);
        <<"set">> ->
            TS = proplists:get_value(<<"timestamp">>, Op),
            Value = proplists:get_value(<<"value">>, Op),
            yasa:set(Key, Value, TS);
        <<"incr">> ->
            TS = proplists:get_value(<<"timestamp">>, Op),
            Value = proplists:get_value(<<"value">>, Op),
            yasa:incr(Key, Value, TS)
    end,
    Reply = process_raw_reply(RawReply),
    {reply, {text, Reply}, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
terminate(_Req, _State) ->
    ok.
%%%%%%%%%%%%%%%%%%%%
% Internal Functions
%%%%%%%%%%%%%%%%%%%%

process_raw_reply(ok) -> <<"ok">>;
process_raw_reply(Values) when is_list(Values) -> jsx:to_json(Values).