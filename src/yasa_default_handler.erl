%%%-------------------------------------------------------------------
%%% @doc
%%% Serves files in the application webroot /priv/www
%%% @end
%%%-------------------------------------------------------------------
-module(yasa_default_handler).
-export([init/3, handle/2, terminate/2]).

-define(DEFAULT_HEADER, [{'Content-Type', "text/html; charset=utf-8"}]).

init({tcp,http}, Req, _Opts) -> 
	{ok, Req, undefined_state}.

handle(Req, State) ->
    {Path, _} = cowboy_http_req:path(Req),
    handle_path(Path, Req, State).

handle_path([], Req, State) ->
	Path = [yasa_app:priv_dir(), "/www/dashboardws.html"],
	{ok, Reply} = file:read_file(Path),
	{ok, Req2} = cowboy_http_req:reply(200, ?DEFAULT_HEADER, Reply, Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.