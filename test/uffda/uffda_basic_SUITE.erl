%% @doc
%%   Uses common_test suites to drive PropEr testing of uffda.
%%   The following properties are tested:
%%
%%   <ol>
%%     <li>Register / unregister followed by which_services reports correctly.</li>
%%     <li>Starting up / restarting can't last forever.</li>
%%     <li>Any sequence of up / down / crash / restart should reflect proper state.</li>
%%     <li>Any series of uffda_client calls shouldn't crash regardless of order.</li>
%%     <li>A service supervised without restart should reflect down/crashed when M of N services are killed.</li>
%%     <li>A service supervised with R restarts should reflect state changes with repeated failure and M of N services are killed.</li>
%%   </ol>
%% @end
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
         proper_state_sequence/1,
         proper_random_seq/1,
         proper_valid_events/1,
         proper_timeout_test/1
        ]).

-export([
         create_service/1,
         startup/2
        ]).

-include("uffda_common_test.hrl").

-spec all() -> [module()].
%% @doc
%%   All testcases that are run.
%% @end
all() -> [
    easy,
    easy,
    proc,
    crash,
    name_checks,
    name_sanity,
    proper_sanity,
    proper_state_sequence,
    proper_random_seq,
    proper_valid_events,
    proper_name_checks,
    proper_timeout_test
    ].

-type config() :: proplists:proplist().

-spec init_per_suite(config()) -> config().
%% @doc
%%   One time initialization before executing all testcases in this suite.
%% @end
init_per_suite(Config) -> Config.

-spec end_per_suite(config()) -> config().
%% @doc
%%   One time cleanup after executing all testcases in this suite.
%% @end
end_per_suite(Config) -> Config.

-spec init_per_testcase(module(), config()) -> config().
%% @doc
%%   Initialization before executing each testcase in this suite.
%% @end
init_per_testcase(_TestCase, Config) ->
    ok = uffda:start(),
    Config.

-spec end_per_testcase(module(), config()) -> ok.
%% @doc
%%   Cleanup after executing each testcase in this suite.
%% @end
end_per_testcase(_TestCase, _Config) ->
    uffda:stop().

-spec easy(term()) -> ok.
%% @doc
%%   Simple testing of register, online, offline and status reporting.
%% @end
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
%% @hidden
%% @doc
%%   Spawn a new test service process to monitor.
%% @end
create_service(Name) -> spawn(?MODULE, startup, [Name, self()]).

-spec startup(atom(), pid()) -> term().
%% @hidden
%% @doc
%%   Register and starting up actions for a local test service.
%% @end
startup(Name, Caller) ->
    ok = uffda_client:register_service(Name),
    Caller ! {ok, Name},
    ok = uffda_client:starting_service(Name, self()),
    service_loop(Name).

-spec service_loop(atom()) -> term().
%% @hidden
%% @doc
%%   Receive loop for local test service to induce service events.
%% @end
service_loop(Name) ->
    _ = receive
        die   -> exit(kill);
        up    -> ok = uffda_client:set_service_online(Name);
        down  -> ok = uffda_client:set_service_offline(Name);
        {state, Pid} -> Pid ! uffda_client:service_status(Name)
    end,
    service_loop(Name).

-spec expect_msg(term()) -> ok | notok | term().
%% @hidden
%% @doc
%%   Collect one message from the incoming queue and verify it is
%%   the response that was expected.
%% @end
expect_msg(Msg) ->
    receive
        Msg -> ok;
        Other -> Other
    after
        50 -> notok
    end.

-spec proc(term()) -> ok.
%% @doc
%%   Verify that the local test process works correctly.
%% @end
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
%% @doc
%%   Testing that crashing a test service is reflected by the service FSM.
%% @end
crash(_Config) ->
    ct:log("Test messaging to the service_registry when service crashes"),
    Foo = create_service(foo),
    ok = expect_msg({ok, foo}),
    Foo ! {state, self()},
    ok = expect_msg(starting_up),
    starting_up = uffda_client:service_status(foo),
    Ref = monitor(process, Foo),
    Foo ! die,
    ok = receive
        {'DOWN', Ref, process, _, _} -> ok;
        _ -> unexpected
        end,
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

-spec proper_sanity(term()) -> ok.
%% @doc
%%   Validate that any atom can be used to register a service.
%% @end
proper_sanity(_Config) ->
    ct:log("A new fsm always has registered status."),
    ok = uffda_client:register_service('0'),
    registered = uffda_client:service_status('0'),
    Test_Down_Init =
        ?FORALL(Name, ?SUCHTHAT(Name, atom(), Name =/= ''),
            ?TIMEOUT(50,
                begin
                    ok = uffda_client:register_service(Name),
                    registered =:= uffda_client:service_status(Name)
                end)),
    true = proper:quickcheck(Test_Down_Init, ?PQ_NUM(10)),
    ok.

transition() ->
    union([set_service_online, set_service_offline]).
-spec proper_state_sequence(term()) -> ok.
%% @doc
%%   Verify that 2 transitions don't cause a problem.
%% @end
proper_state_sequence(_Config) ->
    ct:log("Testing states do not get confuddled given a random transition sequence."),
    ok = uffda_client:register_service('foo'),
    ok = uffda_client:starting_service('foo', self()),
    ok = uffda_client:set_service_online('foo'), 
    Up_Down_Seq =
        ?FORALL({T1, T2}, tuple([transition(), transition()]),
          ?TIMEOUT(50, 
            begin      
               ok = erlang:apply(uffda_client, T1, ['foo']),
               ok = erlang:apply(uffda_client, T2, ['foo']),
               ?WHENFAIL(ct:log("T1:~p~nT2:~p", [T1, T2]), 
                   case T2 of
                       set_service_online -> 
                            up =:= uffda_client:service_status('foo');
                       set_service_offline -> 
                            down =:= uffda_client:service_status('foo')
                   end)
        end)),
    true = proper:quickcheck(Up_Down_Seq, ?PQ_NUM(10)),
    ok.

more_trans() ->
    union([starting_service, transition()]).

-spec proper_random_seq(term()) -> ok.
%% @doc
%%   Verify that any sequence of transitions reflect the correct final state.
%% @end
proper_random_seq(_Config) ->
    ct:log("Testing more complex transition sequences."),
    ok = uffda_client:register_service('foo'),
    ok = uffda_client:starting_service('foo', self()),
    ok = uffda_client:set_service_online('foo'),
    Rand_Seq = ?FORALL(Transition_List, list(more_trans()),
        ?IMPLIES((length(Transition_List) < 20) and (length(Transition_List) > 0), begin
            New_State =
                lists:foldl(fun(Action, _) ->
                                Args = case Action of
                                    starting_service -> ['foo', self()];
                                    _ -> ['foo']
                                end,
                                apply(uffda_client, Action, Args),
                                uffda_client:service_status('foo')
                            end,
                            uffda_client:service_status('foo'), Transition_List),
            ?WHENFAIL(ct:log("TList: ~p", [Transition_List]),
                case lists:last(Transition_List) of
                    set_service_offline -> down =:= New_State;
                    set_service_online -> up =:= New_State;
                    starting_service -> (starting_up =:= New_State) or (restarting =:= New_State)
                end)
        end)),
    true = proper:quickcheck(Rand_Seq, ?PQ_NUM(10)),
    ok.

valid_events() ->
    union([unregister_service, register_service, more_trans()]).
proper_valid_events(_Config) ->
    ct:log("Testing that any set of valid events won't crash FSM"),
    ok = uffda_client:register_service(foo),
    Valid_Events = 
        ?FORALL(Event, valid_events(),
            ?WHENFAIL(ct:log("~p", Event), 
                begin
                    Result = case Event of
                        starting_service ->
                            uffda_client:starting_service(foo, self());
                        _ ->
                            uffda_client:Event(foo)
                    end,
                    lists:member(Result,
                        [ok, {error, {not_registered, foo}}, {error, already_started}])
                end)),
    true = proper:quickcheck(Valid_Events, ?PQ_NUM(10)),
    ok.

-spec name_checks(term()) -> ok.
%% @doc
%%   Register/unregister produces a valid set.
%% @end
name_checks(_Config) ->
    ct:log("Names expected are names returned."),
    [] = uffda_client:which_services(),
    ok = uffda_client:register_service(baz),
    [baz] = uffda_client:which_services(),
    ok = uffda_client:register_service(boop),
    true = ordsets:from_list([baz, boop]) == 
           ordsets:from_list(uffda_client:which_services()),
    ok = uffda_client:unregister_service(baz),
    true = ordsets:from_list([boop]) == ordsets:from_list(uffda_client:which_services()),
    ok = uffda_client:unregister_service(boop),
    [] = uffda_client:which_services(),
    ct:comment("A basic way to check names~n").

%% @private
balance_check([], _Reg, UnReg) ->
    _ = [ok = uffda_client:unregister_service(Un) || Un <- UnReg],
    true;
balance_check([H|T], Reg, UnReg) ->
    case random:uniform(3) of
        1 -> ok = uffda_client:register_service(H),
             NewReg = ordsets:add_element(H, Reg),
             true = ordsets:is_element(H, NewReg),
             true = ordsets:is_subset(Reg, NewReg),
             Registered_Actual = uffda_client:which_services(),
             NewReg = ordsets:from_list(Registered_Actual),
             balance_check(T, NewReg, [H | UnReg]);
        2 when UnReg /= [] -> Index = random:uniform(length(UnReg)),
             Service = lists:nth(Index, UnReg),
             NewUnReg = lists:delete(Service, UnReg),
             NewReg = ordsets:del_element(Service, Reg),
             ok = uffda_client:unregister_service(Service),
             Registered_Actual = uffda_client:which_services(),
             NewReg = ordsets:from_list(Registered_Actual),
             balance_check([H|T], NewReg, NewUnReg);
        2 -> balance_check([H|T], Reg, UnReg);
        3 -> ct:sleep(10), 
             balance_check([H|T], Reg, UnReg)
    end.

-spec name_sanity(term()) -> ok.
%% @doc
%%   Check that state is properly cleaned up.
%% @end
name_sanity(_Config) ->
    Names = ['','+¬b!Vd','õ\026','\037þ\020o×3]\d×','\210','=',
                     '-\235\b\bM','¾\036'],
    Run = fun() -> [] = uffda_client:which_services(),
                   true = balance_check(Names, ordsets:new(), [])
                   end,
    _ = [Run() || _ <- lists:seq(1, 5)],
    ct:comment("A basic check that state is properly cleaned up.").

-spec proper_name_checks(term()) -> ok.
%% @doc
%%   Any number of register/unregisters returns the proper registered set of names.
%% @end
proper_name_checks(_Config) ->
    ct:log("Registered services are the expected ones."),
    NCs = ?FORALL(NameList, list(atom()), 
            ?IMPLIES((10 < length(NameList)) and (length(NameList) < 200),
              ?TIMEOUT(5000,
                  begin
                      UniqueNameList = ordsets:to_list(ordsets:from_list(NameList)),
                      ?WHENFAIL(ct:log("UNL: ~p", [UniqueNameList]), 
                        balance_check(UniqueNameList, ordsets:new(), []))
                  end))),
    true = proper:quickcheck(NCs, ?PQ_NUM(3)),
    ct:comment("Tested that registering maintains the available names properly.").

proper_timeout_test(_Config) ->
    ct:log("Verifies starting up won't last forever."),
    Timeout =
        ?FORALL(Time, pos_integer(),
                ?IMPLIES((Time < 50) and (Time > 15),
                         begin
                             create_sleepy_service(foo, Time),
                             Result = slow_start =:= uffda_client:service_status(foo),
                             ok =uffda_client:unregister_service(foo),
                             Result
                         end)),
    true = proper:quickcheck(Timeout, ?PQ_NUM(10)),
    ok.

-spec create_sleepy_service(atom(), integer()) -> ok.
create_sleepy_service(Name, Time) ->
    ok = uffda_client:register_service(Name, [{max_startup_millis, 14}]),
    ok = uffda_client:starting_service(Name, self()),
    ct:sleep(Time),
    ok.
 
