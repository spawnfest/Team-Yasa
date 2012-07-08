-module(yasa_ws_api_handler).

-export([init/4, stream/3, info/3, terminate/2]).

-record(state, {
	registered = [] :: [binary()]
}).

-define(TICKER, 59000).

init(_Transport, Req, _Opts, _Active) ->
	erlang:start_timer(?TICKER, self(), tick),
    {ok, Req, #state{}}.

stream(<<"ping">>, Req, State) ->
	{reply, <<"pong">>, Req, State};
stream(Data, Req, State = #state{registered = Registered}) ->
	Json = jsx:to_term(Data),
	Type = proplists:get_value(<<"method">>, Json),
	Key = proplists:get_value(<<"key">>, Json),
	case Type of 
		<<"unregister">> ->
    		{reply, <<"ok">>, Req, State#state{registered = lists:delete(Key, Registered)}};
    	<<"register">> ->
    		{200, Values} = yasa_handler_utils:reply(Key, <<"get">>, Json),
    		Reply = [{key, Key}, {values, Values}],
    		{reply, jsx:to_json([Reply]), Req, State#state{registered = [Key | Registered]}};
    	<<"set">> -> 
    		yasa_handler_utils:reply(Key, <<"set">>, Json),
    		{reply, <<"ok">>, Req, State};
    	<<"incr">> -> 
    		yasa_handler_utils:reply(Key, <<"incr">>, Json),
    		{reply, <<"ok">>, Req, State};
    	Type ->
    		{reply, <<"error: invalid request">>, Req, State}
    end.

info({timeout, _TRef, tick}, Req, State = #state{registered = Registered}) ->
	erlang:start_timer(?TICKER, self(), tick),	
	Mapper = fun(Key) ->
		{200, Values} = yasa_handler_utils:reply(Key, <<"get">>, [{<<"range">>, <<"-1min">>}]),
		[{key, Key}, {values, Values}]
	end,
	Data = lists:map(Mapper, Registered),
	{reply, jsx:to_json(Data), Req, State};
info(_Info, Req, State) ->
	io:format("~p~n", [_Info]), 
    {ok, Req, State}.

terminate(_Req, _State) ->
    ok.