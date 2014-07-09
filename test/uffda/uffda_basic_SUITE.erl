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
        proper_state_sequence/1,
        startup_timeout/1,
        proper_random_seq/1
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
    proper_state_sequence,
    startup_timeout,
    proper_random_seq
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
    registered = uffda_client:service_status(foo),
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
        die   -> exit(kill);
        up    -> uffda_client:set_service_online(Name);
        down  -> uffda_client:set_service_offline(Name);
        reset -> uffda_client:reset_service(Name);
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
    ok = expect_msg(registered),
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
    ok = expect_msg(registered),
    registered = uffda_client:service_status(foo),
    Foo ! die,
    erlang:yield(),
    false = is_process_alive(Foo),
    crashed = uffda_client:service_status(foo),
    Bar = create_service(bar),
    ok = expect_msg({ok, bar}),
    Bar ! up,
    Bar ! {state, self()},
    ok = expect_msg(up),
    up = uffda_client:service_status(bar),
    Bar ! die,
    erlang:yield(),
    crashed = uffda_client:service_status(bar),
    ct:comment("Tested FSM reaction to a crashing function service"),
    ok.

proper_sanity(_Config) ->
    ct:log("A new fsm is always in the 'STARTING_UP' state."),
    ok = uffda_client:register_service('0'),
    registered = uffda_client:service_status('0'),
    Test_Down_Init =
        ?FORALL(Name, ?SUCHTHAT(Name, atom(), Name =/= ''),
                begin
                    ok = uffda_client:register_service(Name),
                    registered =:= uffda_client:service_status(Name)
                end),
    true = proper:quickcheck(Test_Down_Init, ?PQ_NUM(10)),
    ok.

-type transition() :: set_service_online | set_service_offline.
proper_state_sequence(_Config) ->
    ct:log("Testing states do not get confuddled given a random transition sequence."),
    uffda_client:register_service('foo'),
    uffda_client:starting_service('foo'),
    uffda_client:set_service_online('foo'), 
    Up_Down_Seq =
        ?FORALL([T1, T2], [transition(), transition()], begin
           ok = erlang:apply(uffda_client, T1, ['foo']),
           ok = erlang:apply(uffda_client, T2, ['foo']),
           ct:log("~p", [uffda_client:service_status('foo')]),
           case T2 of
               set_service_online -> up =:= uffda_client:service_status('foo');
               set_service_offline -> down =:= uffda_client:service_status('foo')
           end
        end),
    true = proper:quickcheck(Up_Down_Seq, ?PQ_NUM(10)),
    ok.

-type more_trans() :: starting_service | transition().
proper_random_seq(_Config) ->
    ct:log("Testing more complex transition sequences."),
    uffda_client:register('foo'),
    uffda_client:starting_service('foo'),
    Rand_Seq = ?FORALL(Transition_List, list(more_trans()),
        ?IMPLIES(length(Transition_List) < 20, begin
            [First | _] = Transition_List,
            lists:foldl(fun(Action, _) -> apply(uffda_client, Action, 'foo') end, First, Transition_List),
            New_State = uffda_client:service_status('foo'),
            case lists:last(Transition_List) of
                set_service_offline -> down =:= New_State;
                set_service_online -> up =:= New_State;
                starting_service -> (starting_up =:= New_State) or (restarting =:= New_State)
            end
        end)),
    true = proper:quickcheck(Rand_Seq, ?PQ_NUM(10)),
    ok.

startup_timeout(_Config) ->
    ct:log("STATE_STARTING_UP should timeout to DELAYED_START, and RESTARTING should
           timeout to DELAYED_RESTART"),
    uffda_client:register_service('foo'),
    uffda_client:starting_service('foo'),
    timer:sleep(1),
    slow_start = uffda_client:service_status('foo'),
    uffda_client:set_service_offline('foo'),
    uffda_client:starting_service('foo'),
    restarting = uffda_client:service_status('foo'),
    timer:sleep(1),
    slow_restart = uffda_client:service_status('foo'),
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
