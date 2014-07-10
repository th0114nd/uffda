%% @doc
%%   The service suite tests the ability of uffda to monitor active services.
%%   We use property-based testing to validate that the registry maintains
%%   an accurate reflection of services' current status.
%%
%%   The following properties must hold true for any implementaton of an
%%   uffda service monitor to be correct:
%%
%%   <ol>
%%     <li>No crash occurs when any sequence of uffda_client calls are made.</li>
%%     <li>.</li>
%%     <li>.</li>
%%   </ol>
%% @end
-module(uffda_service_SUITE).
-vsn('').

-export([all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/1, end_per_group/1,
         init_per_testcase/2, end_per_testcase/2
        ]).

-export([
         stress_no_service/1, stress_one_service/1, stress_many_services/1
        ]).


-include("uffda_common_test.hrl").
-type test_group() :: atom().

-spec all() -> [{group, test_group()}].
%% @doc
%%   All testcase groups that are run.
%% @end
all() -> [{group, no_crash}, {group, no_crash}].

-spec groups() -> [test_group()].
%% @doc
%%   Testcases are grouped so that a failure can save time.
%% @end
groups() -> [
             {no_crash, [sequence], [stress_no_service, stress_one_service, stress_many_services]}
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
init_per_testcase(_TestCase, Config) ->
    ok = uffda:start(),
    Config.

-spec end_per_testcase(module(), config()) -> config().
%% @doc
%%   Cleanup after executing each testcase in this suite.
%% @end
end_per_testcase(_TestCase, _Config) ->
    uffda:stop().

-spec stress_no_service(config()) -> true.
%% @doc
%%   Guarantee that starting the uffda application creates an
%%   uffda service registry, and that stopping the application
%%   removes the service registry.
%% @end
stress_no_service(_Config) ->
    true.

-spec stress_one_service(config()) -> true.
%% @doc
%%   Guarantee that starting the uffda application creates an
%%   uffda service registry, and that stopping the application
%%   removes the service registry.
%% @end
stress_one_service(_Config) ->
    true.

-spec stress_many_services(config()) -> true.
%% @doc
%%   Guarantee that starting the uffda application creates an
%%   uffda service registry, and that stopping the application
%%   removes the service registry.
%% @end
stress_many_services(_Config) ->
    true.
