-module(uffda_up_down_model).
-behavior(tcb_model).

-include_lib("test_commons/include/tcb.hrl").
%% Generate tcb_model and expected outcomes.
-export([get_all_test_model_ids/0,
         generate_model/2,
         deduce_expected_status/1]).

%% Used per scenario when validating against the common behavior model.
-export([vivify_scenario/1,
        translate_scenario_dsl/1,
        translate_scenario_events/1,
        generate_observation/2,
        passed_test_case/3]).


%% Returns a list of test model ids.
-spec get_all_test_model_ids() -> [tcb_model_id()].%, tc_model_source()].
get_all_test_model_ids() -> [].

%% Generates a common behavior model from an id and a source.
-spec generate_model(tcb_model_id(), tcb_model_source()) ->
    tcb_model().
generate_model(_Id, _Source) ->
    #tcb_model{}.

%%------------------------------------------------------------------------
%% SIMULATING A TRANSITION SEQUENCE
%%------------------------------------------------------------------------

%% Extracts supervision tree and events from DSL.
extract_tree_and_events(FileRead) ->
    {ok, {{startup, Tree}, {actions, Events}}} = FileRead,
    {Tree, Events}.

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
transition({{leaf, {supervisor, _}}, _} = Program) -> Program;
transition({{leaf, Leaf}, Events}) ->
    {worker, {Name, ex_worker, Status}} = Leaf,
    Actions = proplists:get_all_values(Name, Events),
    FinalState = lists:foldl(fun(Event, Acc) -> state_change(Acc, Event) end,
                Status, Actions),
    {leaf, {worker, {Name, ex_worker, FinalState}}};
transition({{node, Parent, Children}, Events}) ->
    translate_tree({{node, Parent, Children}, Events}).

%% Evaluates Events for immediate children of the root of Tree.
translate_tree({Tree, Events}) -> 
    {node, Parent, Children} = Tree,
    NewTree = {node, Parent, lists:map(fun(Child) -> transition({Child, Events}) end, Children)},
    {NewTree, Events}.

-spec deduce_expected_status(tcb_scenario()) -> term(). 
deduce_expected_status(#tcb_scenario{scenario_desc = Scenario} = _TCPS) ->
    translate_tree(extract_tree_and_events(Scenario)).

%%---------------------------------------------------------------------
%% A LIVE TRANSITION SEQUENCE
%%---------------------------------------------------------------------

%% Uses DSL to instantiate a real program.
-spec vivify_scenario(tcb_scenario()) -> tcb_scenario_live_ref().
vivify_scenario(_Scenario) ->
    #tcb_scenario{}. 

-spec translate_scenario_dsl(tcb_scenario_dsl_desc()) -> 
    tcb_scenario_live_desc().
translate_scenario_dsl(_DSL_Desc) -> ok.

-spec translate_scenario_events(tcb_scenario_dsl_events()) ->
    tcb_scenario_live_events().
translate_scenario_events(_DSL_Events) -> ok.

%% Generates an observation by running events on live program.
-spec generate_observation(tcb_scenario_live_ref(), tcb_test_case()) -> term().
generate_observation(_Live_Model_Ref, #tcb_test_case{} = _Test_Case_Instance) ->
    success.

%% Compares the expected results with live program results.
-spec passed_test_case(pos_integer(), tcb_scenario_dsl_status(),
                              tcb_scenario_live_status()) -> boolean().
passed_test_case(_CaseNumber, _ExpectedStatus, _ObservedStatus) -> true.
