-module(yasa).
-export([start/0, set/2, incr/2, get/3, keys/0]).

%%===================================================================
%% Public API
%%===================================================================
-spec start() -> 'ok' | {'error',_}.
start() ->
    application:start(cowboy),
    application:start(yasa).

%%%----------------------------------
%%% @doc
%%% Sets the value of the gauge with the given key by value
%%% @end
%%%----------------------------------
-spec set(binary(), integer()) -> ok.
set(Key, Value) ->
    send(set, Key, Value).

%%%----------------------------------
%%% @doc
%%% Increments the value of the counter with the given key by incr
%%% @end
%%%----------------------------------
-spec incr(binary(), integer()) -> ok.
incr(Key, Incr) ->
    send(incr, Key, Incr).

%%%----------------------------------
%%% @doc
%%% Returns the values for the given key between 
%%% Start and End timestamps
%%% @end
%%%----------------------------------
-spec get(binary(), integer(), integer()) -> list().
get(Key, Start, End) ->
    send(get, Key, Start, End).

%%%----------------------------------
%%% @doc
%%% Returns all keys being stored by YASA
%%% eg. [<<"stat.keyname">>, ...]
%%% @end
%%%----------------------------------
-spec keys() -> list().
keys() ->
    yasa_rra_file:get_keys().

%%===================================================================
%% Internal
%%===================================================================
send(Type, Key, Value) ->
    {ok, Pid} = lookup(Key, Type),
    gen_server:call(Pid, {Type, Value}).

send(Type, Key, Start, End) ->
    {ok, Pid} = lookup(Key, Type),
    gen_server:call(Pid, {Type, Start, End}).

lookup(Key, Type) ->
    case yasa_pid_store:lookup(Key) of
        {ok, Pid} ->
            {ok, Pid};
        {error, not_found} ->
            create(Key, Type)
    end.

create(Key, Type) ->
    {ok, Pid} = Reply = yasa_rra_sup:start_child(Key, Type),
    yasa_pid_store:insert(Key, Pid),
    Reply.
