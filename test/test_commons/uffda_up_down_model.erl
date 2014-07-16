-module(uffda_up_down_model).
-behavior(tc_proper_model).

%% Generate tc_proper_model and expected outcomes.
-export([get_all_test_model_ids/0,
         generate_proper_model/2,
         deduce_proper_expected_status/1]).

%% Used per scenario when validating against the proper model.
-export[vivify_proper_scenario/1,
        translate_proper_scenario/1,
        translate_proper_scenario_dsl/1,
        translate_proper_scenario_events/1,
        generate_proper_observation/1,
        passed_proper_test_case/3]).


%% Returns a list of test model ids.
-spec get_all_test_model_ids() -> [tc_proper_model_id(), tc_proper_model_source()].
get_all_test_model_ids() -> [].

%% Generates a proper model from an id and a source.
-spec generate_proper_model(tc_proper_model_id(), tc_proper_model_source()) ->
    tc_proper_model().
generate_proper_model(Id, Source) ->
    #tc_proper_model{}.

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
transition({{leaf, Leaf}, Events}) ->
    {worker, {Name, ex_worker, Status}} = Leaf,
    Actions = proplists:get_all_values(Name, Events),
    FinalState = lists:foldl(fun(Event, Acc) -> state_change(Acc, Event) end,
                Status, Actions),
    {leaf, {worker, {Name, ex_worker, FinalState}}};
transition({{leaf, {supervisor, _}}, _} = Program) -> Program;
transition({{node, Parent, Children}, Events}) ->
    translate_tree({{node, Parent, Children}, Events}).

%% Evaluates Events for immediate children of the root of Tree.
translate_tree({Tree, Events}) -> 
    {node, Parent, Children} = Tree,
    NewTree = {node, Parent, lists:map(fun(Child) -> transition({Child, Events}) end, Children)},
    {NewTree, Events}.

-spec deduce_proper_expected_status(tc_proper_scenario()) -> term(). 
deduce_proper_expeced_status(#tc_proper_scenario{scenario_desc = Scenario} = TCPS) ->
    translate_tree(extract_tree_and_events(Scenario)).

%%---------------------------------------------------------------------
%% A LIVE TRANSITION SEQUENCE
%%---------------------------------------------------------------------

%% Uses DSL to instantiate a real program.
-spec vivify_proper_scenario(tc_proper_scenario()) -> tc_proper_scenario_live_ref().
vivify_proper_scenario(Scenario) ->
    #tc_proper_scenario{}. 

-spec translate_proper_scenario_dsl(tc_proper_scenario_dsl_desc()) -> 
    tc_proper_scenario_live_desc().
translate_proper_scenario_dsl(DSL_Desc) -> ok.

-spec translate_proper_scenario_events(tc_proper_scenario_dsl_events()) ->
    tc_proper_scenario_live_events().
translate_proper_scenario_events(DSL_Events) -> ok.

%% Generates an observation by running events on live program.
-spec generate_proper_observation(tc_proper_test_case()) -> term().
generate_proper_observation(TestCaseInstance) -> ok.

%% Compares the expected results with live program results.
-spec passed_proper_test_case(pos_integer(), tc_proper_scenario_dsl_status(),
                              tc_proper_scenario_live_status()) -> boolean().
passed_proper_test_case(CaseNumber, ExpectedStatus, ObservedStatus) -> true.
