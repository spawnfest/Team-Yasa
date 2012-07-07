%%%-------------------------------------------------------------------
%%% @doc
%%% Contains the interface for interacting with the YASA RRA files
%%% @end
%%%-------------------------------------------------------------------
-module(yasa_rra_file).
-export([load_from_file/1, save_to_file/2]).

load_from_file(Path) when is_binary(Path) ->
	load_from_file(binary_to_list(Path));
load_from_file(Path) when is_list(Path) ->
    case file:read_file(Path) of
        {ok, Binary} ->  binary_to_term(Binary);
        {error, enoent} ->  {error, not_found};
        {error, Error} -> {error, Error}
    end.

%%%----------------------------------
%%% @doc
%%% Dump retention tables to disk
%%% @end
%%%----------------------------------
save_to_file(Path, {Type, Retentions, RQS}) ->
    R = file:write_file(Path, term_to_binary({Type, Retentions, RQS})),
    io:format("~p     ~p~n", [Path, R]).

%%%===================================================================
%%% Internal functions
%%%===================================================================