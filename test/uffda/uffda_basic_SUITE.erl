-module(uffda_basic_SUITE).
-vsn('').

-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).
-export([
        easy/1
        ]).

-export([
    create_service/1,
    service_loop/1
    ]).

-include("uffda_common_test.hrl").

all() -> [
    easy,
    easy
    ].

init_per_suite(Config) -> Config.
end_per_suite(Config) -> Config.

init_per_testcase(_TestCase, Config) ->
    ok = uffda:start(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    uffda:stop().

-spec easy(term()) -> ok.
easy(_Config) -> 
    {ok, _Pid} = sr_client:register_me(foo),
    'STARTING_UP' = sr_client:get_state(foo),
    ok = sr_client:go_up(foo),
    'UP' = sr_client:get_state(foo),
    ok = sr_client:go_down(foo),
    'DOWN' = sr_client:get_state(foo),
    ok = sr_client:reset(foo),
    'STARTING_UP' = sr_client:get_state(foo),
    ok.

-spec create_service(atom()) -> pid().
create_service(Name) -> spawn(?MODULE, fun service_loop/1, [Name]).


-spec service_loop(atom()) -> term().
service_loop(Name) ->
    receive
        die -> exit(told_to);
        reg -> sr_client:register_me(Name);
        up -> sr_client:go_up(Name);
        down -> sr_client:go_down(Name);
        reset -> sr_client:reset(Name);
        {state, Pid} -> Pid ! sr_client:get_state(Name)
    end,
    service_loop(Name).
