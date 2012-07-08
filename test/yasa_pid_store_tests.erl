-module(yasa_pid_store_tests).
-include_lib("eunit/include/eunit.hrl").

yasa_pid_store_test() ->
    Pid = spawn(fun() -> receive _Block -> ok_dead end end), 
    yasa_pid_store:start_link(),

    % should insert key with pid
    ?assertEqual(ok, yasa_pid_store:insert(unique_key, Pid)),
    ?assertMatch({ok, Pid}, yasa_pid_store:lookup(unique_key)),

    % should delete a key-to-pid mapping on process death
    ?assertEqual(true, exit(Pid, die)),
    timer:sleep(10),
    ?assertEqual({error, not_found}, yasa_pid_store:lookup(unique_key)).
