-module(yasa_basic_handler).

-export([init/3, handle/2, terminate/2]).

init({tcp,http}, Req, _Opts) -> 
	{ok, Req, undefined_state}.

handle(Req, State) ->
	Path = [yasa_app:priv_dir(), "/www/demo.html"],
	{ok, Reply} = file:read_file(Path),
	ReplyHeader =  [{'Content-Type', "text/html; charset=utf-8"}],
	{ok, Req2} = cowboy_http_req:reply(200, ReplyHeader, Reply, Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.