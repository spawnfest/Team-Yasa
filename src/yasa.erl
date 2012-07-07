-module(yasa).
-export([start/0, set/2, incr/2, get/3, keys/0]).

%%===================================================================
%% Public API
%%===================================================================
start() ->
    application:start(cowboy),
    application:start(yasa).

set(Key, Value) ->
    send(set, Key, Value).

incr(Key, Value) ->
    send(incr, Key, Value).

get(Key, Start, End) ->
    send(get, Key, Start, End).

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
