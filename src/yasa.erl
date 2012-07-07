-module(yasa).
-export([start/0, set/3, incr/3, get/3]).

%%===================================================================
%% Public API
%%===================================================================
start() ->
    application:start(cowboy),
    application:start(yasa).

set(Key, Timestamp, Value) ->
    send(set, Key, Timestamp, Value).

incr(Key, Timestamp, Value) ->
    send(incr, Key, Timestamp, Value).

get(Key, Start, End) ->
    send(get, Key, Start, End).

%%===================================================================
%% Internal
%%===================================================================

send(Type, Key, Arg1, Arg2) ->
    {ok, Pid} = lookup(Key, Type),
    gen_server:call(Pid, {Type, Arg1, Arg2}).

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
