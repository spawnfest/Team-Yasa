% @doc This process stores key-to-pid mappings in its state. It is used
-module(yasa_pid_store).
-behaviour(gen_server).

%% API
-export([start_link/0,
         insert/2,
         lookup/1
         ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    lookup_table :: dict() % stores key-to-pid mappings
}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec lookup(term()) -> {ok, pid()}|{error, not_found}.
lookup(Key) ->
    gen_server:call(?SERVER, {lookup, Key}).

-spec insert(term(), pid()) -> ok.
insert(Key, Pid) ->
    gen_server:call(?SERVER, {insert, {Key, Pid}}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, #state{ lookup_table = dict:new() }}.

handle_call({insert, {Key, Pid}}, _From, #state{ lookup_table = LookupTable } = State) ->
    erlang:monitor(process, Pid),
    NewTable = dict:store(Key, Pid, LookupTable),
    {reply, ok, State#state{ lookup_table = NewTable }};

handle_call({lookup, Key}, _From, #state{ lookup_table = LookupTable } = State) ->
    Reply = case dict:find(Key, LookupTable) of
        error -> {error,not_found};
        Item -> Item
    end,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

% When a process dies we need to de-register it
handle_info({'DOWN', MonitorRef, process, Pid, _Info}, #state{ lookup_table = LookupTable } = State) ->
    erlang:demonitor( MonitorRef ),
    Key = reverse_pid_lookup(Pid, LookupTable),
    NewTable = dict:erase(Key, LookupTable),
    {noreply, State#state{ lookup_table = NewTable }};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

reverse_pid_lookup(Pid, Table) ->
    [{Key,Pid}] = dict:to_list(
        dict:filter(
            fun(_,V) ->
                V =:= Pid
            end, Table
        )
    ),
    Key.
