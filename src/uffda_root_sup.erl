-module(uffda_root_sup).
-behavior(supervisor).

-export([start_link/1, init/1, run_prog/0]).

-define(SUPER, ?MODULE).
-define(CHILD(__Mod, __Args), {__Mod, {__Mod, start_link, __Args},
                               permanent, 2000, worker, [__Mod]}).
-spec start_link(Include_Testing :: boolean()) -> {ok, pid()}.
start_link(IT) 
  when is_boolean(IT) ->
    supervisor:start_link({local, ?SUPER}, ?MODULE, IT).

-spec init(Include_Testing :: boolean()) -> {ok, {{supervisor:strategy(), non_neg_integer(), non_neg_integer()},
                                                  [supervisor:child_specs()]}}.
init(true) ->
    Procs = [{uffda_sup_sup, {?MODULE, run_prog, []}, transient, 2000, worker, [uffda_sup_sup]}],
    init_act(Procs);
init(false) -> 
    init_act([]).

-spec init_act([supervisor:child_specs()]) -> {ok, {{supervisor:strategy(), non_neg_integer(), non_neg_integer()},
                                                    [supervisor:child_specs()]}}.
init_act(Procs) ->
    NewProcs = [?CHILD(uffda_registry_sup, []) | Procs],
    {ok, {{one_for_one, 10, 10}, NewProcs}}.

-spec run_prog() -> {ok, pid()}.
run_prog() ->
    {ok, spawn_link(fun() ->  
            {ok, Prog} = uffda_dsl:parse_from_file('priv/ex.prog'),
            uffda_dsl:run_program(Prog),
            receive
                not_really_sent -> ok
            end 
        end)}.
