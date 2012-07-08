-module(yasa_http_api_handler).

%http callbacks
-export([init/3, handle/2, terminate/2]).

-define(l2b(X), list_to_binary(X)).

init({tcp,http}, Req, _Opts) -> 
    {ok, Req, undefined_state}.

handle(Req, State) ->
    http_handle(Req, State).

%
% HTTP Handler
%

http_handle(Req, State) ->
    ReplyHeader = [{'Content-Type', "application/json; charset=utf-8"}],

    {Method, _} = cowboy_http_req:binding(action, Req),
    Key = yasa_handler_utils:pval(<<"key">>, Req),
    Callback = yasa_handler_utils:pval(<<"callback">>, Req), % json-p callback

    {Status, RawReply} = yasa_handler_utils:reply(Key, Method, Req),
    Reply = jsonp(Callback, to_json(RawReply)),

    {ok, Req2} = cowboy_http_req:reply(Status, ReplyHeader, Reply, Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.

% ============================================================================
% Internal Functions
% ============================================================================
jsonp(undefined, Reply) -> Reply;
jsonp(Callback, Reply) ->
    [Callback, <<"(">>, Reply, <<");">>].

to_json(<<"error:invalid request">>) ->
    <<"error:invalid request">>;
to_json(<<"">>)->
    <<"[]">>;
to_json(Term) ->
    jsx:to_json(Term).