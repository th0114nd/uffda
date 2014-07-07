-module(uffda_basic_SUITE).
-vsn('').

-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).
-export([
        easy/1,
        crash/1,
        proc/1,
        proper_sanity/1,
        group_query_checks/1,
        proper_state_sequence/1
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
    crash,
    proper_sanity,
    group_query_checks,
    proper_state_sequence
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
    ct:log("Test basic supervisor / fsm startup and state change capability"),
    ok = uffda_client:register_service(foo),
    starting_up = uffda_client:service_status(foo),
    ok = uffda_client:set_service_online(foo),
    up = uffda_client:service_status(foo),
    ok = uffda_client:set_service_offline(foo),
    down = uffda_client:service_status(foo),
    ct:comment("Tested ~p internal states", [['STARTING_UP', 'UP', 'DOWN', 'STARTING_UP']]),
    ok.

-spec create_service(atom()) -> pid().
create_service(Name) -> spawn(?MODULE, startup, [Name, self()]).

-spec startup(atom(), pid()) -> term().
startup(Name, Caller) ->
    uffda_client:register_service(Name),
    Caller ! {ok, Name},
    service_loop(Name).

-spec service_loop(atom()) -> term().
service_loop(Name) ->
    receive
        die -> exit(kill);
        up -> uffda_client:set_service_online(Name);
        reset -> uffda_client:reset_service(Name);
        down -> uffda_client:set_service_offline(Name);
        {state, Pid} -> Pid ! uffda_client:service_status(Name)
    end,
    service_loop(Name).

-spec expect_msg(term()) -> ok | notok | term().
expect_msg(Msg) ->
    receive
        Msg -> ok;
        Other -> Other
    after
        50 -> notok
    end.

-spec proc(term()) -> ok.
proc(_Config) ->
    ct:log("Test messaging to the service_registry"),
    Foo = create_service(foo),
    ok = expect_msg({ok, foo}),
    Foo ! {state, self()},
    ok = expect_msg(starting_up),
    Foo ! up,
    Foo ! {state, self()},
    ok = expect_msg(up),
    Foo ! down,
    Foo ! {state, self()},
    ok = expect_msg(down),
    ct:comment("Tested FSM reaction to a normally function service"),
    ok.
     
-spec crash(term()) -> ok.
crash(_Config) ->
    ct:log("Test messaging to the service_registry when service crashes"),
    Foo = create_service(foo),
    ok = expect_msg({ok, foo}),
    Foo ! {state, self()},
    ok = expect_msg(starting_up),
    starting_up = uffda_client:service_status(foo),
    Foo ! die,
    true = is_process_alive(Foo),
    starting_up = uffda_client:service_status(foo),
    Bar = create_service(bar),
    ok = expect_msg({ok, bar}),
    Bar ! up,
    Bar ! {state, self()},
    ok = expect_msg(up),
    up = uffda_client:service_status(bar),
    Bar ! die,
    erlang:yield(),
    down = uffda_client:service_status(bar),
    ct:comment("Tested FSM reaction to a crashing function service"),
    ok.

proper_sanity(_Config) ->
    ct:log("A new fsm is always in the 'STARTING_UP' state."),
    ok = uffda_client:register_service('0'),
    starting_up = uffda_client:service_status('0'),
    Test_Down_Init =
        ?FORALL(Name, atom(), begin
                                  ok = uffda_client:register_service(Name),
                                  starting_up =:= uffda_client:service_status(Name)
                              end),
    true = proper:quickcheck(Test_Down_Init, ?PQ_NUM(10)),
    ok.

-type transition() :: set_service_online | set_service_offline.
proper_state_sequence(_Config) ->
    ct:log("Testing states do not get confuddled given a random transition sequence."),
    ok = uffda_client:register_service('foo'),
    'STARTING_UP' = uffda_client:service_status('foo'),
    ok = uffda_client:set_service_online('foo'),
    'UP' = uffda_client:service_status('foo'),
    Up_Down_Seq =
        ?FORALL([T1, T2], [transition(), transition()], begin
           ok = erlang:apply(uffda_client, T1, ['foo']),
           ok = erlang:apply(uffda_client, T2, ['foo']),
           case T2 of
               set_service_online -> 'UP' =:= uffda_client:service_status('foo');
               set_service_offline -> 'DOWN' =:= uffda_client:service_status('foo')
           end
        end),
    true = proper:quickcheck(Up_Down_Seq, ?PQ_NUM(10)),
    ok.

group_query_checks(_Config) ->
    ct:log("Checking that queries run properly."),
    [] = uffda_client:which_services(),
    ok = uffda_client:register_service(baz),
    [Content] = uffda_client:which_services(),
    ok = uffda_client:register_service(boop),
    [_, _] = uffda_client:which_services(),
    true = lists:member(Content, uffda_client:which_services()),
    ok = uffda_client:unregister_service(boop),
    [Content] = uffda_client:which_services().
