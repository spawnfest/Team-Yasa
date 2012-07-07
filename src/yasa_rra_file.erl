%%%-------------------------------------------------------------------
%%% @doc
%%% Contains the interface for interacting with the YASA RRA files
%%% @end
%%%-------------------------------------------------------------------
-module(yasa_rra_file).
-export([load_from_file/3, save_to_file/5]).

-record(archive, {
    schema,
    retentions
}).

load_from_file(Path, Type, Schema) ->
    case file:read_file(Path) of
        {ok, Binary} ->
            #archive{retentions=Retentions, schema=Schema} = binary_to_term(Binary);
        {error, enoent} ->
            {error, not_found}
    end.

%%%----------------------------------
%%% @doc
%%% Dump retention tables to disk
%%% @end
%%%----------------------------------
save_to_file(Type, Schema, P, S, T) ->
    Record = #archive{schema=Schema, retentions=[P, S, T]},
    file:write_file(Path, term_to_binary(Record)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

validate(

compare_retantions(A, A) -> true;
compare_retantions(_, _) -> false.

get_retentions_for_schema(Schema) when is_binary(Schema) ->
    get_retentions_for_schema(binary_to_atom(Schema, utf8));
get_retentions_for_schema(Schema) when is_atom(Schema) ->
    case application:get_env(yasa, Schema) of 
        undefined ->
            get_retentions_for_schema(default);
        {ok, {retentions, Rets}} -> 
            Rets
    end.

check_divisible([{FirstStep, _SampleSize} | Tail]) ->
    check_divisible(Tail, FirstStep).

check_divisible([], _) -> true;
check_divisible([{Step, _Size} | Tail], FirstStep) ->
    case Step rem FirstStep of
        0 -> check_divisible(Tail, FirstStep);
        _ -> error(retention_are_relatively_prime)
    end.
