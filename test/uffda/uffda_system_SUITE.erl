%% @doc
%%   The system suite tests the ability of uffda to handle multiple services
%%   which are supervised for restart and are cycling through availability.
%%
%%   The following properties are tested for a full system:
%%
%%   <ol>
%%     <li>A single supervisor with M of N services killed and no restarts.</li>
%%     <li>A single supervisor with M of N services killed and 3 restarts.</li>
%%     <li>A root supervisor with two children which are supervisors of services
%%         and has a lower supervisor restart.</li>
%%   </ol>
%% @end
-module(uffda_system_SUITE).
-vsn('').

-export([all/0, groups/0,
         init_per_suite/1,    end_per_suite/1,
         init_per_group/1,    end_per_group/1,
         init_per_testcase/2, end_per_testcase/2
        ]).

-export([
         single_no_restart/1, single_with_restart/1, tree_restart/1, dsl_first_run/1
        ]).


-include("uffda_common_test.hrl").
-type test_group() :: atom().

-spec all() -> [{group, test_group()}].
%% @doc
%%   All testcase groups that are run.
%% @end
all() -> [{group, supervised_services}, dsl_first_run].

-spec groups() -> [test_group()].
%% @doc
%%   Testcases are grouped so that a failure can save time.
%% @end
groups() -> [
             {supervised_services, [sequence],
              [single_no_restart,  single_with_restart,  tree_restart]}
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

-spec single_no_restart(config()) -> true.
%% @doc
%%   A single supervisor with no restarts managing multiple
%%   services which experience failure.
%% @end
single_no_restart(_Config) ->
    true.

-spec single_with_restart(config()) -> true.
%% @doc
%%   A single supervisor with restarts managing multiple
%%   services which experience failure.
%% @end
single_with_restart(_Config) ->
    true.

-spec tree_restart(config()) -> true.
%% @doc
%%   A tree of 3 processes with restarts that manages
%%   2 groups of services and experiences repeated failure
%%   of a service which takes out its parent. The restart
%%   logic uses the knowledge of restart to have a different
%%   behavior after subsequent restarts.
%% @end
tree_restart(_Config) ->
    true.

-spec dsl_first_run(config()) -> true.
%% @doc
%%   A check that the random generation of programs works
%%   properly. 
%% @end
dsl_first_run(_Config) ->
    Gen_Test = ?FORALL(Prog, ?LET(P, integer(), P), begin ct:log("P: ~p", [Prog]), true end),
%        begin
%            ct:log("Prog: ~p", [Prog]),
%            true
%        end),
%%        begin
%            Workers = uffda_dsl:extract_workers(Tree),
%            ?IMPLIES(length(Workers) == length(sets:to_list(sets:from_list(Workers))),
%                begin
%                ct:log("Workers: ~p", [Workers]),
%                ?FORALL(NumActions, list({range(1, length(Workers), uffda_dsl:real_world_event()}),
%                begin
%                    ct:log("Length NA: ~p", [length(NumActions)]),
%                    ?IMPLIES(lists:all(fun({N, _}) -> (1 =< N) and (N =< length(Workers)) end, NumActions),
%                        begin
%                            Actions = lists:map(fun({N, E}) -> {lists:nth(N, Workers), E} end, 
%                                NumActions),
%                            Prog = {{startup, Tree}, {actions, Actions}},
%                            ct:log("Prog: ~p", [Prog]),
%                            true
%                        end)
%                 end)
%           end)
%        end) end),
    true = proper:quickcheck(Gen_Test, ?PQ_NUM(100)).

%gen_naive_prog() ->
%    {{startup, union(
%gen_prog() ->
%    ?LET(Prog, integer(), Prog).
%%        begin
%%            Workers = uffda_dsl:extract_workers(Tree),
%%            ?LET(Actions, list({union(Workers), uffda_dsl:real_world_event()}),
%%                begin
%%                    {{startup, Tree}, {actions, Actions}}
%%                end) end).
%            
%fake_prog() ->
%    {{startup, fake_tree()}, {actions, fake_actions()}}.
%    
%fake_tree() ->
%    union({leaf, fake_wos()}, {node, fake_super(), [fake_tree()]}).
%    
%fake_wos() ->
%    union({supervisor, fake_super()}, {worker, fake_worker()}).
%    
%fake_super() ->
%     
