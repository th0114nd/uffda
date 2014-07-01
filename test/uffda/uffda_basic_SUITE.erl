-module(uffda_basic_SUITE).
-vsn('').

-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).
-export([
        easy/1
        ]).

-include("uffda_common_test.hrl").

all() -> [
    easy
    ].

init_per_suite(Config) -> Config.
end_per_suite(Config) -> Config.

init_per_testcase(_TestCase, Config) ->
    Out = uffda:start(),
    case Out of
        ok -> Config;
        _ -> error_logger:error_msg("Out is ~p~n", [Out])
    end.

end_per_testcase(_TestCase, _Config) ->
    Pid = whereis(uffda),
    unlink(Pid),
    uffda:stop().

-spec easy(term()) -> ok.
easy(_Config) -> 
    ct:log("okay"),
    {ok, _Pid} = sr_client:register_me(foo),
    'STARTING_UP' = sr_client:get_state(foo),
    ok = sr_client:go_up(foo),
    'UP' = sr_client:get_state(foo),
    ok = sr_client:go_down(foo),
    'DOWN' = sr_client:get_state(foo),
    ok = sr_client:reset(foo),
    'STARING_UP' = sr_client:get_state(foo),
    ct:log("finished up"),
    true.
