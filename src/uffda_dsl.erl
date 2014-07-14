-module(uffda_dsl).

-include("uffda.hrl").

-export([parse_from_file/1, run_program/1]).
-type program() :: {sup_tree_spec(), [action()]}.
-type sup_tree_spec() :: indiv_spec() | {indiv_spec(), [sup_tree_spec()]}.
-type indiv_spec() :: {indiv, module(), term(), [child_desc()]}.
-type child_desc() :: {{any(), mfargs(), restart(), shutdown(), workorsup(), modules()}, service_status()}.
-type action() :: {service_name(), service_event()}.

-type mfargs() :: {atom(), atom(), [any()]}.
-type restart() :: permanent | transient | temporary.
-type shutdown() :: brutal_kill | pos_integer() | infinity.
-type workorsup() :: worker | supervisor.
-type module() :: atom().
-type modules() :: [module()] | dynamic.
-type reason() :: term().

-type deep_list() :: [char() | atom() | deep_list()].
-type name_all() :: string() | atom() | deep_list() | binary().

-spec parse_from_file(name_all()) -> {ok, program()} | {error, reason()}. 
parse_from_file(File_Name) ->
    {ok, Raw} = file:consult(File_Name),
    [TreeSpec | Actions] = Raw,
    {TreeSpec, Actions}.

-spec run_program(program()) -> ok | {error, reason()}.
run_program({Sup_Tree, Actions}) ->
    % Make sure that all the actions refer to workers present in the tree.
    true = ensure_workers_valid(Sup_Tree, Actions),
    ok = create_sup_tree(Sup_Tree),
    ok = execute_all(Actions).

-spec create_sup_tree(sup_tree_spec()) -> ok.
create_sup_tree({indiv, Module, Args, Children}) ->
    {ok, Sup_Ref} = supervisor:start_link(Module, Args),
    [supervisor:start_child(Sup_Ref, Child_Spec) || {Child_Spec, _} <- Children],
    ok;
create_sup_tree({Node, Below}) ->
    create_sup_tree(Node),
    lists:foreach(fun ?MODULE:create_sup_tree/1, Below).

-spec ensure_workers_valid(sup_tree_spec(), [action()]) -> boolean(). 
ensure_workers_valid(_Sup_Tree, _Actions) ->
    % Wait until further functionality added.
    true.

-spec execute_all([action()]) -> ok.
execute_all([]) -> ok;
execute_all([Act | Acts]) ->
    {Worker, Event} = Act,
    Worker ! Event,
    execute_all(Acts).
