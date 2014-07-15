%%  Proper testing occurs via automated scripting of randomly generated
%%  scenarios. Each application provides its own customized randomization,
%%  application behavior, set of event types, and a predictive engine which
%%  generates the expected solution for a given scenario. A test suite is
%%  the execution of a the automated script against a collection of scenarios.

%%   - Proper Scenario: Scenario + Initial Status + Events => Final Status
%%      - This is a static representation, generated randomly by proper
%%      - 

-type scenario_desc()    :: term().
-type situation_status() :: term().
-type situation_events() :: [term()].

-record(tc_proper_scenario,
        {
          instance = 0    :: non_neg_integer(),
          scenario        :: scenario_desc(),
          initial_status  :: situation_status(),
          events          :: situation_events()
        }).

-type tc_proper_scenario() :: #tc_proper_scenario{}.

-define(TC_MISSING_TEST_CASE_ELEMENT, '$$_not_generated').

-record(tc_proper_test_case,
        {
          scenario        :: tc_proper_scenario(),
          expected_status = ?TC_MISSING_TEST_CASE_ELEMENT :: situation_status() | ?TC_MISSING_TEST_CASE_ELEMENT,
          observed_status = ?TC_MISSING_TEST_CASE_ELEMENT :: situation_status() | ?TC_MISSING_TEST_CASE_ELEMENT
        }).

-type tc_proper_test_case() :: #tc_proper_test_case{}.

-record(tc_proper_model,
        {
          behaviour      :: module(),
          scenarios = [] :: [tc_proper_scenario()]
        }).

-type tc_proper_model() :: #tc_proper_model{}.
