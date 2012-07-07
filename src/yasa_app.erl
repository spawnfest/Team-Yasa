-module(yasa_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, priv_dir/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	ok = start_web_server(),
	ok = check_retentions(),
    yasa_sup:start_link().

stop(_State) ->
    ok.

priv_dir() ->
    case code:priv_dir(yasa) of
        {error, bad_name} ->
            {ok, Cwd} = file:get_cwd(),
            Cwd ++ "/" ++ "priv/";
        Priv ->
            Priv ++ "/"
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_web_server() ->
	Port = case application:get_env(yasa, port) of 
        {ok, P} -> P;
        undefined -> 8080
    end,
	Dispatch = [
	    %% {Host, list({Path, Handler, Opts})}
	    {'_', [
	    	{[<<"api">>, action], yasa_api_handler, []},
	    	{[], yasa_static_handler, []}
		]}
	],
	%% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
	cowboy:start_listener(my_http_listener, 16,
	    cowboy_tcp_transport, [{port, Port}],
	    cowboy_http_protocol, [{dispatch, Dispatch}]
	),
	ok.

check_retentions() ->
    case application:get_env(yasa, retentions) of 
        undefined -> throw("Please define retentions");
        {ok, Rets} ->  check_retention_divisibility(Rets)
    end.

check_retention_divisibility([{FirstStep, _} | Tail]) ->
	Fun = fun({StepSize, _}) ->
		case StepSize rem FirstStep of
			0 -> ok;
			_ -> throw("Retentions are relatively primary")
		end
	end,
	lists:foreach(Fun, Tail). 