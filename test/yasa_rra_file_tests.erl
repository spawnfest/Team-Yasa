-module(yasa_rra_file_tests).
-include_lib("eunit/include/eunit.hrl").

rra_file_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            fun save_load/0,
            fun load_errors/0
            ]}.

        
%%%===================================================================
%%% Setup / Teardown
%%%===================================================================

setup() ->
    ok = filelib:ensure_dir(dir()),
    ok = file:write_file([dir(), "dummy.yasa"], <<>>).

cleanup(_) ->
    ok = file:del_dir(dir()).

%%%===================================================================
%%% Tests
%%%===================================================================

save_load() ->
    yasa_rra_file:save_to_file([dir(), "dummy.yasa"], {foo, bar, baz}),
    Res = yasa_rra_file:load_from_file([dir(), "dummy.yasa"]),
    ?assertEqual({foo, bar, baz}, Res),
    file:delete([dir(), "dummy.yasa"]).

load_errors() ->
    Res = yasa_rra_file:load_from_file("shouldnotexistreally"),
    ?assertEqual({error, not_found}, Res).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

dir() ->	
    [yasa_app:priv_dir(), "/tests/"].
