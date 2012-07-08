-module(yasa_rra_file_tests).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

rra_file_test_() ->
	[{foreach, fun create_dummy_file/0, fun remove_dummy_file/1, 
		[fun test_load/1, fun test_save/1]},
	% {setup, fun create_dummy_tree/0, fun remove_dummy_tree/1, fun test_get_keys/1},
	 test_load_errors()].
%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%

create_dummy_file() ->
	ok = filelib:ensure_dir(dir()),
	file:write_file([dir(), name()], term_to_binary(test_value)).

remove_dummy_file(_) ->
	file:delete([dir(), name()]),
	file:del_dir(yasa_app:priv_dir()).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CLEANUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

create_dummy_tree() ->
	 ok = filelib:ensure_dir([dir(), "/dummy1/"]),
	 ok = filelib:ensure_dir([dir(), "/dummy2/"]),
	 ok = file:write_file([dir(), "/dummy1/s1.yasa"], <<0>>),
	 ok = file:write_file([dir(), "/dummy1/s2.yasa"], <<0>>),
	 ok = file:write_file([dir(), "/dummy2/s3.yasa"], <<0>>),
	 ok = file:write_file([dir(), "/dummy2/s4.yasa"], <<0>>).

remove_dummy_tree(_) ->
	file:delete([dir(), "/dummy1/s1.yasa"]),
	file:delete([dir(), "/dummy1/s2.yasa"]),
	file:delete([dir(), "/dummy2/s3.yasa"]),
	file:delete([dir(), "/dummy2/s4.yasa"]),
	file:del_dir([dir(), "/dummy1/"]),
	file:del_dir([dir(), "/dummy2/"]),
	file:del_dir([dir()]),
	file:del_dir([yasa_app:priv_dir()]).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

test_load(_) ->
	Res = yasa_rra_file:load_from_file([dir(), name()]),
	[?_assertEqual(Res, test_value)].

test_save(_) ->
	yasa_rra_file:save_to_file([dir(), name()], {hmm, 'try', this}),
	Res = yasa_rra_file:load_from_file([dir(), name()]),
	[?_assertEqual(Res, {hmm, 'try', this})].
%%%wrong test
test_get_keys(_) ->
	Keys = yasa_rra_file:get_keys(),
	?debugFmt("Keys: ~p~n", [Keys]),
	Expected = 
		[[	{label,<<"storage">>},
        	{expanded,true},
        	{items,[[{label,<<"dummy1">>},
                 {expanded,true},
                 {items,[[{html,<<"<div class='keyname' data-name='dummy1.s1'><a href='#'>s1</a></div>">>}],
                         [{html,<<"<div class='keyname' data-name='dummy1.s2'><a href='#'>s2</a></div>">>}]]}],
                [{label,<<"dummy2">>},
                 {expanded,true},
                 {items,[[{html,<<"<div class='keyname' data-name='dummy2.s3'><a href='#'>s3</a></div>">>}],
                         [{html,<<"<div class='keyname' data-name='dummy2.s4'><a href='#'>s4</a></div>">>}]]}]]}]]
	[?_assertEqual(Keys, Expected)].

test_load_errors() ->
	Res = yasa_rra_file:load_from_file(["readmefromreverse!namnom'c"]),
	Res2 = yasa_rra_file:load_from_file("adirectory/"),
	file:del_dir("adirectory/"),
	[?_assertEqual(Res, {error, not_found}),
	 ?_assertMatch({error, _}, Res2),
	 ?_assertNotEqual(Res2, {error, not_found})].

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

name() ->
	"dummy.yasa".
dir() ->
	[yasa_app:priv_dir(), "/storage/"].