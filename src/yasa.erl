-module(yasa).
-export([set/3, incr/3, get/3]).

%%===================================================================
%% Public API
%%===================================================================

set(Key, Timestamp, Value) ->
    send(set, Key, Timestamp, Value).

incr(Key, Timestamp, Value) ->
    send(incr, Key, Timestamp, Value).

get(Key, Start, End) ->
    ok.

%%===================================================================
%% Internal
%%===================================================================

send(Type, Key, Timestamp, Value) ->
    case lookup(Key, Type) of
        {error, Reason} ->
            {error, Reason};
        {ok, Pid} ->
            gen_server:call(Pid, {Type, Timestamp, Value})
    end.

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
