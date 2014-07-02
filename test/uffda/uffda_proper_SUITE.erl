-module(uffda_proper_SUITE).
-vsn('').
-include("uffda_common_test.hrl").
-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([basic/1]).

all() -> [basic].

init_per_suite(Config) -> Config.
end_per_suite(Config) -> Config.

init_per_testcase(TestCase, Config) ->
    uffda_basic_SUITE:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    uffda_basic_SUITE:end_per_testcaes(TestCase, Config).

%% Testcases.
basic(_Config) ->
    ct:log("A new fsm is always in the down state."),
    Test_Down_Init =
        ?FORALL(Name, atom(), begin
                                  ok = sr_client:register_me(Name),
                                  'DOWN' =:= sr_client:get_state(Name)
                              end),
    true = proper:quickcheck(Test_Down_Init, ?PQ_NUM(10)).
