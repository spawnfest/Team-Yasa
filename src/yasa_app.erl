-module(yasa_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, priv_dir/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start(_,_) ->  {'error',_} | {'ok',pid()}.
start(_StartType, _StartArgs) ->
	ok = start_web_server(),
	% don't start with out properly defined retentions
	ok = check_retentions(),
    yasa_sup:start_link().

-spec stop(_) -> 'ok'.
stop(_State) ->
    ok.

%%%----------------------------------
%%% @doc
%%% returns the priv dir for yasa application
%%% @end
%%%----------------------------------
-spec priv_dir() -> nonempty_string().
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

%% @private start the web server(cowboy) on the specified port of 8080
%% by default
start_web_server() ->
	Port = case application:get_env(yasa, port) of 
        {ok, P} -> P;
        undefined -> 8080
    end,
	Dispatch = [
	    %% {Host, list({Path, Handler, Opts})}
	    {'_', [
	    	{[<<"api">>, action], yasa_api_handler, []},
	    	{[<<"assets">>, '...'], cowboy_http_static,
    			[{directory, {priv_dir, yasa, [<<"www/assets">>]}},
    			{mimetypes, [
          			{<<".css">>, [<<"text/css">>]},
          			{<<".js">>, [<<"application/javascript">>]}]}]},
	    	{[], yasa_default_handler, []}
		]}
	],
	%% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
	cowboy:start_listener(my_http_listener, 16,
	    cowboy_tcp_transport, [{port, Port}],
	    cowboy_http_protocol, [{dispatch, Dispatch}]
	),
	ok.

%% @private check if the retentions are define in config file and make
%% sure the are not relatively primary
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
