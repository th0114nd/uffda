-module(tree_processing).
-export([extract_tree_and_events/1]).

-type not_program() :: {uffda_dsl:sup_tree_spec(), [uffda_dsl:action()]}.
%% Extracts supervision tree and events list from uffda_dsl:parse_from_file.
extract_tree_and_events(FileRead) ->
    {ok, {{startup, Tree}, {actions, Events}}} = FileRead,
    translate_tree({Tree, Events}).

%% Symbolically calculates final worker states.
-spec state_change(atom(), term()) -> atom().
state_change(_, online) -> up;
state_change(_, offline) -> down;
state_change(_, {starting, _}) -> starting_up;
state_change(_, {re_init, _}) -> restarting;
state_change(_, unregister) -> killed;
state_change(killed, _) -> killed;
state_change(State, _) -> State.

%% Applies state_change to leaf node or evaluates subtree.
-spec transition(not_program()) -> not_program().
transition({{leaf, {supervisor, _}}, _} = Prog) -> Prog;
transition({{leaf, Leaf}, Events}) ->
    {worker, {Name, ex_worker, Status}} = Leaf,
    Actions = proplists:get_all_values(Name, Events),
    FinalState = lists:foldl(fun(Event, Acc) -> state_change(Acc, Event) end,
                Status, Actions),
    {leaf, {worker, {Name, ex_worker, FinalState}}};
transition({{node, Parent, Children}, Events}) ->
    translate_tree({{node, Parent, Children}, Events}).

%% Evaluates Events for immediate children of the root of Tree.
-spec translate_tree(not_program()) -> not_program().
translate_tree({Tree, Events}) -> 
    {node, Parent, Children} = Tree,
    NewTree = {node, Parent, lists:map(fun(Child) -> transition({Child, Events}) end, Children)},
    {NewTree, Events}.
 
