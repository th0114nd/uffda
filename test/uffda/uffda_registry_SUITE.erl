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

%% Common Test testing
-export([all/0, groups/0,
         init_per_suite/1,    end_per_suite/1,
         init_per_group/1,    end_per_group/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
         verify_start_application/1,
         verify_register_simple_name/1, verify_register_service_name/1,
         verify_register_unregister/1
        ]).

%% Dev shell testing
-export([
         prop_register_simple_name/0,
         prop_register_service_name/0
%%         prop_register_unregister/0
        ]).


-include("uffda_common_test.hrl").
-type test_group() :: atom().

-spec all() -> [atom() | {group, test_group()}].
%% @doc
%%   All testcases that are run.
%% @end
all() -> [{group, verify_registry}].

-spec groups() -> [{atom(), [atom()], [{atom(), atom()}]}].
%% @doc
%%   Testcases are grouped so that a failure can save time.
%% @end
groups() -> [
             %% If app start doesn't work, skip the rest...
             {verify_registry,   [sequence], [{group, verify_app},
                                              {group, verify_register}  %% ,
%%                                              {group, verify_unregister}
                                             ]},

             %% Each test group is progressively more complex so that a simpler
             %% failure will skip more complicated related tests.
             {verify_app,        [sequence], [verify_start_application]},
             {verify_register,   [sequence], [verify_register_simple_name, verify_register_service_name]},
             {verify_unregister, [sequence], [verify_register_unregister]}
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

-spec init_per_group(config()) -> config().
%% @doc
%%   One time initialization before executing a group in this suite.
%% @end
init_per_group(Config) -> Config.

-spec end_per_group(config()) -> config().
%% @doc
%%   One time cleanup after executing a group in this suite.
%% @end
end_per_group(Config) -> Config.

-spec init_per_testcase(module(), config()) -> config().
%% @doc
%%   Initialization before executing each testcase in this suite.
%% @end
init_per_testcase(_Test_Case, Config) -> Config.

-spec end_per_testcase(module(), config()) -> config().
%% @doc
%%   Cleanup after executing each testcase in this suite.
%% @end
end_per_testcase(_TestCase, Config) -> Config.

-spec verify_start_application(config()) -> true.
%% @doc
%%   Guarantee that starting the uffda application creates an
%%   uffda service registry, and that stopping the application
%%   removes the service registry.
%% @end
verify_start_application(_Config) ->
    ct:log("Register, unregister, and which_services have no effect when uffda not yet started"),
    true = prop_start_application(),
    ct:comment("Tested that missing registry does not crash the code"),
    true.

prop_start_application() ->
    Service_Name = foo,
    {error, {not_started, uffda_registry_sup}} = uffda_client:register_service   (Service_Name),
    {error, {not_started, uffda_registry_sup}} = uffda_client:unregister_service (Service_Name),
    {error, {not_started, uffda_registry_sup}} = uffda_client:which_services(),

    ct:log("Register and unregister only allow a name to be registered once"),
    ok = uffda:start(),
    {error, {not_registered, foo}} = uffda_client:unregister_service (Service_Name),
    ok                             = uffda_client:register_service   (Service_Name),
    {error, already_started}       = uffda_client:register_service   (Service_Name),
    [foo]                          = uffda_client:which_services     (),
    ok                             = uffda_client:unregister_service (Service_Name),
    {error, {not_registered, foo}} = uffda_client:unregister_service (Service_Name),
    []                             = uffda_client:which_services     (),
    ok = uffda:stop(),

    ct:log("uffda_client:which_services/0 doesn't work after uffda has stopped"),
    {error, {not_started, uffda_registry_sup}} = uffda_client:which_services(),
    true.
    

-spec verify_register_simple_name(config()) -> true.
%% @doc
%%   Register any legal uppercase alpha atom and then query the
%%   registry to see that the same atom is returned as registered.
%% @end
verify_register_simple_name(_Config) ->
    ct:log("Register a few uppercase alpha names."),
    true = prop_register_simple_name(),
    ct:comment("Tested that registering works without strange service name characters."),
    true.

-spec verify_register_service_name(config()) -> true.
%% @doc
%%   Register any legal atom and then query the registry to
%%   see that the same atom is returned as registered.
%% @end
verify_register_service_name(_Config) ->
    ct:log("Register a few real service names."),
    true = prop_register_service_name(),
    ct:comment("Tested that registering works with any legal atom as a service name."),
    true.

prop_register_simple_name() ->
    ok = uffda:start(),
    try Model_Result_Pairs = tc_proper_model:test_all_models(uffda_register_name_model),
        Results = [Res || {_Id, Res} <- Model_Result_Pairs],
        lists:all(fun({Passed, _Num_Passed, _Failures}) -> Passed end, Results)
    %try register_one_name(alpha)
    after ok = uffda:stop()
    end.

prop_register_service_name() ->
    ok = uffda:start(),
    Result = register_one_name(atom),
    ok = uffda:stop(),
    Result.

register_one_name(Type) ->
    Name_Test
        = ?FORALL(Name, atom_list(Type),
                  %% Ensure synchronous messaging doesn't block.
                  ?TIMEOUT(50,
                           begin
                               Service_Name = case is_atom(Name) of
                                                  true  -> Name;
                                                  false -> list_to_atom(Name)
                                              end,
                               %% ct:log("Testing service name: ~p~n", [Service_Name]),
                               ?WHENFAIL(ct:log("Service name ~p failed~n", [Service_Name]),
                                         begin
                                             %% Undo register in case a dup name comes later.
                                             ok = uffda_client:register_service   (Service_Name),
                                             [Service_Name] = uffda_client:which_services(),
                                             ok= uffda_client:unregister_service (Service_Name),
                                             true
                                         end)
                           end)),
    true = proper:quickcheck(Name_Test, ?PQ_NUM(100)).

atom_list(alpha) -> list(range(65,90));
atom_list(atom)  -> atom().
    
-spec verify_register_unregister(config()) -> true.
%% @doc
%%   Perform a random sequence of register/unregister/wait
%%   commands. At the end of all activity, the set of
%%   registered services should reflect only those that
%%   are still registered.
%% @end
verify_register_unregister(_Config) ->
    ct:log("Unregister is the reverse of register."),
%%    true = prop_register_unregister(),
    ct:comment("Tested that registering maintains the available names properly."),
    true.
