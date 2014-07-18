%%  Proper testing occurs via automated scripting of randomly generated
%%  scenarios. Each application provides its own customized randomization,
%%  application behavior, set of event types, and a predictive engine which
%%  generates the expected solution for a given scenario. A test suite is
%%  the execution of the automated script against a collection of scenarios.

%% All types used to express the DSL description of a scenario
-type tc_proper_scenario_dsl_desc()    :: term().
-type tc_proper_scenario_dsl_status()  :: term().
-type tc_proper_scenario_dsl_events()  :: [term()].

%% All types used to express the live implementation of a scenario
-type tc_proper_scenario_live_ref()    :: term().
-type tc_proper_scenario_live_desc()   :: term().
-type tc_proper_scenario_live_status() :: term().
-type tc_proper_scenario_live_events() :: [term()].

%% An instance of a scenario description using DSL
-record(tc_proper_scenario,
        {
          instance = 0     :: non_neg_integer(),                % Scenario instance id
          scenario_desc    :: tc_proper_scenario_dsl_desc(),    % Description of the scenario
          initial_status   :: tc_proper_scenario_dsl_status(),  % Initial status for the scenario
          events           :: tc_proper_scenario_dsl_events()   % Set of events to occur during test
        }).

-type tc_proper_scenario() :: #tc_proper_scenario{}.

%% An test case is a scenario augmented with expected and observered statuses
-define(TC_MISSING_TEST_CASE_ELEMENT, '$$_not_generated').

-record(tc_proper_test_case,
        {
          scenario        :: tc_proper_scenario(),
          expected_status  = ?TC_MISSING_TEST_CASE_ELEMENT :: tc_proper_scenario_dsl_status()
                                                            | ?TC_MISSING_TEST_CASE_ELEMENT,
          observed_status  = ?TC_MISSING_TEST_CASE_ELEMENT :: tc_proper_scenario_live_status()
                                                            | ?TC_MISSING_TEST_CASE_ELEMENT
        }).

-type tc_proper_test_case() :: #tc_proper_test_case{}.

-type tc_proper_model_id()     :: term().
-type tc_proper_model_source() :: {file, file:name_all()}
                                | {mfa, {Module::module(), Function::atom(), Args::list()}}.

%% The full PropEr model
-record(tc_proper_model,
        {
          id             :: tc_proper_model_id(),      % Unique identifier
          source         :: tc_proper_model_source(),  % Source of the model
          behaviour      :: module(),                  % Implementation of the tc_proper_model behaviour
          scenarios = [] :: [tc_proper_scenario()]     % A set of scenarios to be tested
        }).

-type tc_proper_model()        :: #tc_proper_model{}.
-type tc_proper_model_result() :: {boolean(), Number_Of_Passed_Scenarios :: pos_integer(),
                                   Failed_Scenarios :: [tc_proper_scenario()]}.
