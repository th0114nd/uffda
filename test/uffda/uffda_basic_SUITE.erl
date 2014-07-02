-module(uffda_basic_SUITE).
-vsn('').

-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).
-export([
        easy/1,
        crash/1,
        proc/1
        ]).

-export([
    create_service/1,
    startup/2
    ]).

-include("uffda_common_test.hrl").

all() -> [
    easy,
    easy,
    proc,
    crash
    ].

init_per_suite(Config) -> Config.
end_per_suite(Config) -> Config.

init_per_testcase(_TestCase, Config) ->
    ok = uffda:start(),
    Config.

end_per_testcase(_TestCase, _Config) ->
  %  {ok, Pid} = sr_client:register_me(endcase),
  %  Mref = monitor(process, Pid),
    uffda:stop().

-spec easy(term()) -> ok.
easy(_Config) -> 
    ok = sr_client:register_me(foo),
    'STARTING_UP' = sr_client:get_state(foo),
    ok = sr_client:go_up(foo),
    'UP' = sr_client:get_state(foo),
    ok = sr_client:go_down(foo),
    'DOWN' = sr_client:get_state(foo),
    ok = sr_client:reset(foo),
    'STARTING_UP' = sr_client:get_state(foo),
    ok.

-spec create_service(atom()) -> pid().
create_service(Name) -> spawn(?MODULE, startup, [Name, self()]).

-spec startup(atom(), pid()) -> term().
startup(Name, Caller) ->
    sr_client:register_me(Name),
    Caller ! {ok, Name},
    service_loop(Name).

-spec service_loop(atom()) -> term().
service_loop(Name) ->
    receive
        die -> exit(kill);
        up -> sr_client:go_up(Name);
        down -> sr_client:go_down(Name);
        reset -> sr_client:reset(Name);
        {state, Pid} -> Pid ! sr_client:get_state(Name)
    end,
    service_loop(Name).

-spec expect_msg(term()) -> ok | notok.
expect_msg(Msg) ->
    receive
        Msg -> ok;
        Other -> Other
    after
        500 -> notok
    end.

-spec proc(term()) -> ok.
proc(_Config) ->
    Foo = create_service(foo),
    ok = expect_msg({ok, foo}),
    Foo ! {state, self()},
    ok = expect_msg('STARTING_UP'),
    Foo ! up,
    Foo ! {state, self()},
    ok = expect_msg('UP'),
    Foo ! down,
    Foo ! {state, self()},
    ok = expect_msg('DOWN'),
    Foo ! reset,
    Foo ! {state, self()},
    ok = expect_msg('STARTING_UP'),
    'STARTING_UP' = sr_client:get_state(foo).
     
-spec crash(term()) -> ok.
crash(_Config) ->
    Foo = create_service(foo),
    ok = expect_msg({ok, foo}),
    Foo ! {state, self()},
    ok = expect_msg('STARTING_UP'),
    'STARTING_UP' = sr_client:get_state(foo),
    Foo ! die,
    true = is_process_alive(Foo),
    'STARTING_UP' = sr_client:get_state(foo),
    Bar = create_service(bar),
    ok = expect_msg({ok, bar}),
    Bar ! up,
    Bar ! {state, self()},
    ok = expect_msg('UP'),
    'UP' = sr_client:get_state(bar),
    Bar ! die,
    erlang:yield(),
    'DOWN' = sr_client:get_state(bar).
