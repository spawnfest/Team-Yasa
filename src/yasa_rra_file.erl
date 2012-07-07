%%%-------------------------------------------------------------------
%%% @doc
%%% Contains the interface for interacting with the YASA RRA files
%%% @end
%%%-------------------------------------------------------------------
-module(yasa_rra_file).
-export([load_from_file/1, save_to_file/2, get_keys/0]).

%%%----------------------------------
%%% @doc
%%% Read retention table from the given path
%%% @end
%%%----------------------------------
-spec load_from_file([atom() | [any()] | char()]) -> any().
load_from_file(Path) when is_list(Path) ->
	ok = filelib:ensure_dir(Path), 
    case file:read_file(Path) of
        {ok, Binary} ->  binary_to_term(Binary);
        {error, enoent} ->  {error, not_found};
        {error, Error} -> {error, Error}
    end.

%%%----------------------------------
%%% @doc
%%% Dump retention table to given path
%%% @end
%%%----------------------------------
-spec save_to_file(atom() | binary() | [atom() | [any()] | char()],{_,_,_}) -> 'ok' | {'error',atom()}.
save_to_file(Path, {Type, Retentions, RQS}) ->
    file:write_file(Path, term_to_binary({Type, Retentions, RQS})).

%%%----------------------------------
%%% @doc
%%% List all the keys stored in storage dir
%%% @end
%%%----------------------------------

-spec get_keys() -> list().
get_keys() ->
	Root = [yasa_app:priv_dir(), "storage/*"],
	[walk_directory_tree(Root)].

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @private walk the directory at the given root 
%% in a head recursive
walk_directory_tree(Root) ->
	SubTree = lists:map(fun(Elem) ->
		case filelib:is_dir(Elem) of
			true -> walk_directory_tree([Elem, "/*"]);
			false -> get_name_from_path(Elem)
		end
	end,filelib:wildcard(Root)),
	Name = get_name_from_path(Root),
	{Name, SubTree}.


%% @private get the name of the folder or file from path
get_name_from_path(Path) when is_list(Path)->
	FlatPath = lists:flatten(Path), 
	{match, [Name]} = re:run(FlatPath, <<"(/\\w+)*/(?<NAME>\\w+)(/\\*)?">>,
		[{capture, ['NAME'], binary}]),
	Name.