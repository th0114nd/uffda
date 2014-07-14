-module(uffda_dsl).

-include("uffda.hrl").

-type program() :: {sup_tree_spec(), [{service_name(), service_event()}]}.
-type sup_tree_spec() :: indiv_spec() | {indiv_spec(), [sup_tree_spec()]}.
-type indiv_spec() :: [child_desc()].
-type child_desc() :: {{any(), mfargs(), restart(), shutdown(), workorsup(), modules()}, service_status()}.

-type mfargs() :: {atom(), atom(), [any()]}.
-type restart() :: permanent | transient | temporary.
-type shutdown() :: brutal_kill | pos_integer() | infinity.
-type workorsup() :: worker | supervisor.
-type modules() :: [atom()] | dynamic.


-spec transition(program()) -> program().
transition({{Root, Children}, Events}) ->

-spec translate_tree(program()) -> program().
translate_tree({Tree, Events}) ->
    transition(Tree, Events)
