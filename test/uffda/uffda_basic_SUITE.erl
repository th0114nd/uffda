-module(uffda_basic_SUITE).
-vsn('').

-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).
-export([
        easy/1,
        crash/1,
        balance_check/3,
        name_checks/1,
        name_sanity/1,
        proc/1,
        proper_name_checks/1,
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
    name_checks,
    name_sanity,
    proper_sanity,
    group_query_checks,
    proper_state_sequence,
    proper_name_checks
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
    uffda_client:starting_service(Name, self()),
    service_loop(Name).

-spec service_loop(atom()) -> term().
service_loop(Name) ->
    receive
        die   -> exit(kill);
        up    -> uffda_client:set_service_online(Name);
        down  -> uffda_client:set_service_offline(Name);
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
    ct:log("A new fsm always has starting_up status."),
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
    ok = uffda_client:register_service('foo'),
    registered = uffda_client:service_status('foo'),
    ok = uffda_client:set_service_online('foo'),
    up = uffda_client:service_status('foo'),
    Up_Down_Seq =
        ?FORALL([T1, T2], [transition(), transition()], begin
           ok = erlang:apply(uffda_client, T1, ['foo']),
           ok = erlang:apply(uffda_client, T2, ['foo']),
           case T2 of
               set_service_online  -> up   =:= uffda_client:service_status('foo');
               set_service_offline -> down =:= uffda_client:service_status('foo')
           end
        end),
    true = proper:quickcheck(Up_Down_Seq, ?PQ_NUM(10)),
    ok.

group_query_checks(_Config) ->
    ct:log("Checking that queries run properly."),
    [] = uffda_client:which_service_pids(),
    ok = uffda_client:register_service(baz),
    [Content] = uffda_client:which_service_pids(),
    ok = uffda_client:register_service(boop),
    [_, _] = uffda_client:which_service_pids(),
    true = lists:member(Content, uffda_client:which_service_pids()),
    ok = uffda_client:unregister_service(boop),
    [Content] = uffda_client:which_service_pids().

name_checks(_Config) ->
    ct:log("Names expected are names returned."),
    [] = uffda_client:which_service_names(),
    ok = uffda_client:register_service(baz),
    [baz] = uffda_client:which_service_names(),
    ok = uffda_client:register_service(boop),
    true = ordsets:from_list([baz, boop]) == 
           ordsets:from_list(uffda_client:which_service_names()),
    ok = uffda_client:unregister_service(baz),
    true = ordsets:from_list([boop]) == ordsets:from_list(uffda_client:which_service_names()),
    ok = uffda_client:unregister_service(boop),
    [] = uffda_client:which_service_names().

balance_check([], _Reg, UnReg) ->
    [ok = uffda_client:unregister_service(Un) || Un <- UnReg],
    true;
balance_check([H|T], Reg, UnReg) ->
    case random:uniform(3) of
        1 -> ok = uffda_client:register_service(H),
             NewReg = ordsets:add_element(H, Reg),
             true = ordsets:is_element(H, NewReg),
             true = ordsets:is_subset(Reg, NewReg),
             Registered_Actual = uffda_client:which_service_names(),
             case NewReg == ordsets:from_list(Registered_Actual) of
                false -> ct:log("NewRegList: ~p~n names: ~p~n", 
                    [lists:sort(ordsets:to_list(NewReg)), lists:sort(Registered_Actual)]),
                         false = true;
                _ -> true
             end,
             balance_check(T, NewReg, [H | UnReg]);
        2 when UnReg /= [] -> Index = random:uniform(length(UnReg)),
             Service = lists:nth(Index, UnReg),
             NewUnReg = lists:delete(Service, UnReg),
             NewReg = ordsets:del_element(Service, Reg),
             ok = uffda_client:unregister_service(Service),
             Registered_Actual = uffda_client:which_service_names(),
             ct:log("NR: ~p~n RA: ~p~n", [NewReg, Registered_Actual]),
             NewReg == ordsets:from_list(Registered_Actual),
             balance_check([H|T], NewReg, NewUnReg);
        2 -> balance_check([H|T], Reg, UnReg);
        3 -> ct:sleep(10), 
             balance_check([H|T], Reg, UnReg)
    end.

name_sanity(_Config) ->
    Names = ['','+¬b!Vd','õ\026','\037þ\020o×3]\d×','\210','=',
                     '-\235\b\bM','¾\036'],
    Run = fun() -> [] = uffda_client:which_service_names(),
                   true = balance_check(Names, ordsets:new(), [])
                   end,
    [Run() || _ <- lists:seq(1, 100)].

proper_name_checks(_Config) ->
    ct:log("Registered services are the expected ones."),
    NCs = ?FORALL(NameList, list(atom()), 
            ?IMPLIES(length(NameList) < 10, 
                      begin
                          UniqueNameList = ordsets:to_list(ordsets:from_list(NameList)),
                          true = balance_check(UniqueNameList, ordsets:new(), [])
                      end)),
    true = proper:quickcheck(NCs, ?PQ_NUM(100)).
