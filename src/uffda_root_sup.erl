-module(uffda_root_sup).
-behavior(supervisor).

-export([start_link/1, init/1, run_prog/0]).
-export([service_registry/0, prog_tree_test/0, rest_api/0]).
-define(SUPER, ?MODULE).
-define(CHILD(__Mod, __Args), {__Mod, {__Mod, start_link, __Args},
                               permanent, 2000, worker, [__Mod]}).
-spec start_link([atom()]) -> {ok, pid()}.
start_link(Enabled) 
  when is_list(Enabled) ->
    supervisor:start_link({local, ?SUPER}, ?MODULE, Enabled).

-spec init(Enabled :: [atom()]) -> {ok, {{supervisor:strategy(), non_neg_integer(), non_neg_integer()},
                                                  [supervisor:child_spec()]}}.
init(Enabled) 
  when is_list(Enabled) ->
    Procs = [?MODULE:Enable() || Enable <- Enabled],
    {ok, {{one_for_one, 10, 10}, Procs}}.

-spec service_registry() -> supervisor:child_spec().
service_registry() ->
    ?CHILD(uffda_registry_sup, []).

-spec rest_api() -> supervisor:child_spec().
rest_api() ->
    ?CHILD(uffda_api_sup, []).

-spec prog_tree_test() -> supervisor:child_spec().
prog_tree_test() ->
    {?MODULE, {?MODULE, run_prog, []}, transient, 2000, worker, [?MODULE]}.

-spec run_prog() -> {ok, pid()}.
run_prog() ->
    {ok, spawn_link(fun() ->  
            {ok, Prog} = uffda_dsl:parse_from_file('priv/ex.prog'),
            uffda_dsl:run_program(Prog),
            receive
                not_really_sent -> ok
            end
        end)}.
