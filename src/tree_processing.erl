-module(tree_processing).
-export([translate_tree/1]).

-inlcude("uffda_dsl.erl").

%% Symbolically calculates final worker states.
-spec state_change(atom(), term()) -> atom().
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
    FinalState = lists:foldl(fun(Event, Acc) -> state_change(Acc, Event) end,
                Status, Events),
    {{worker, {Name, ex_worker, FinalState}}, Events};
transition({{node, Parent, Children}, Events}) ->
    translate_tree({{node, Parent, Children}, Events}).

-spec translate_tree(program()) -> program() | ok.
translate_tree({Tree, Events}) -> 
    {node, Parent, Children} = Tree,
    NewTree = lists:map(fun(Child) -> ?MODULE:transition({Child, Events}) end, Children),
    {NewTree, Events}.
 
