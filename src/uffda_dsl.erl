-module(uffda_dsl).
-export([parse_fom_file/1, run_program/1]).
-include("uffda.hrl").

-type program() :: {sup_tree_spec(), [action()]}.
-type sup_tree_spec() :: {leaf, wos()} | {node, wos(), [sup_tree_spec()]}.
-type wos() :: {worker, worker_desc()} | {supervisor, super_desc()}.
% In general, {atom(), Module, Args}
-type super_desc() :: {atom(), ex_super, term()}.
-type worker_desc() :: {atom(), ex_worker, service_status()}.
-type action() :: {service_name(), service_event()}.

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
    % Wait until further functionality added to implement.
    true.

-spec execute_all([action()]) -> ok.
execute_all([]) -> ok;
execute_all([Act | Acts]) ->
    {Worker, Event} = Act,
    Worker ! Event,
    execute_all(Acts).

%% Symbolically calculate final worker states.
-spec state_schange(atom(), term()) -> atom().
state_change(_, online) -> up;
state_change(_, offline) -> down;
%% Worry about initial states for STARTING/RESTARTING???
state_change(_, {starting, _}) -> starting_up;
state_change(_, {re_init, _}) -> restarting;
state_change(_, unregister) -> killed;
state_change(killed, _) -> killed.

-spec transition(program()) -> program() | ok.
transition({{leaf, Leaf}, Events}) ->
    {worker, {Name, ex_worker, Status}} = Leaf,
    Actions = proplist:get_all_values(Name, Events),
    FinalState = lists:foldl(fun(Event, Acc) -> state_change(Acc, Event) end),
                Status, Events),
    {{worker, {Name, ex_worker, FinalState}}, Events};
transition({{node, Parent, Children}, Events}) ->
    translate_tree({{node, Parent, Children}, Events}).

-spec translate_tree(program()) -> program() | ok.
translate_tree({Tree, Events}) -> 
    {node, Parent, Children} = Tree,
    NewTree = lists:map(fun(Child) -> ?MODULE:transition({Child, Events}), Children),
    {NewTree, Events}.
 
