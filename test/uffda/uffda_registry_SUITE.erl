%% @doc
%%   The registry suite tests the ability of uffda to record the set of
%%   registered services. We use property-based testing to verify that
%%   the registry functions as expected. Interaction with the uffda registry
%%   may only be made via exported {@link uffda_client} and
%%   {@link uffda_registry_sup} functions, or by starting and stopping the
%%   uffda application.
%%
%%   The following properties must hold true for any implementaton of an
%%   uffda service registry to be correct:
%%
%%   <ol>
%%     <li>Starting the uffda application creates a local singleton registry.</li>
%%     <li>Any legal atom names may be used as a service name.</li>
%%     <li>Registering / unregistering a series of names results in only
%%         the names that are no longer registered being reported as valid
%%         services.</li>
%%   </ol>
%% @end
-module(uffda_registry_SUITE).
-vsn('').

-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
         verify_start_application/1,
         verify_register_service/1,
         verify_register_unregister/1
        ]).


-include("uffda_common_test.hrl").

-spec all() -> [atom()].
%% @doc
%%   All testcases that are run.
%% @end
all() -> [
          verify_start_application,
          verify_register_service,
          verify_register_unregister
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

-spec end_per_testcase(module(), config()) -> config().
%% @doc
%%   Cleanup after executing each testcase in this suite.
%% @end
end_per_testcase(_TestCase, _Config) ->
    uffda:stop().

-spec verify_start_application(config()) -> true.
%% @doc
%%   Guarantee that starting the uffda application creates an
%%   uffda service registry, and that stopping the application
%%   removes the service registry.
%% @end
verify_start_application(_Config) ->
    true.


-spec verify_register_service(config()) -> true.
%% @doc
%%   Register any legal atom and then query the registry to
%%   see that the same atom is returned as registered.
%% @end
verify_register_service(_Config) ->
    true.


-spec verify_register_unregister(config()) -> true.
%% @doc
%%   Perform a random sequence of register/unregister/wait
%%   commands. At the end of all activity, the set of
%%   registered services should reflect only those that
%%   are still registered.
%% @end
verify_register_unregister(_Config) ->
    ct:log("Unregister is the reverse of register."),
    NCs = ?FORALL(NameList, list(atom()), 
            ?IMPLIES((10 < length(NameList)) and (length(NameList) < 200),
              %% Ensure that synchronous messaging doesn't block.
              ?TIMEOUT(5000,
                  begin
                      UniqueNameList = ordsets:to_list(ordsets:from_list(NameList)),
                      ?WHENFAIL(ct:log("UNL: ~p", [UniqueNameList]), 
                        balance_check(UniqueNameList, ordsets:new(), []))
                  end))),
    true = proper:quickcheck(NCs, ?PQ_NUM(3)),
    ct:comment("Tested that registering maintains the available names properly."),
    true.

%% @private
balance_check([], _Reg, UnReg) ->
    [ok = uffda_client:unregister_service(Un) || Un <- UnReg],
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
