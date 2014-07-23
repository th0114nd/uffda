-module(devapi_example).
-behaviour(tcb_model).

-export([
         get_all_test_model_ids/0,
         deduce_expected_status/1,
         vivify_scenario/1,
         transform_raw_scenario/2,
         translate_scenario_dsl/1,
         translate_scenario_events/1,
         generate_observation/2,
         passed_test_case/3
        ]).

-include_lib("test_commons/include/tcb.hrl").

-spec get_all_test_model_ids() -> [{Model_Id :: tcb_model_id(), Source :: tcb_model_source()}].
get_all_test_model_ids() ->
    [{devapi_example, {file, "devapi_example"}}].

-spec deduce_expected_status(Scenario_Instance :: tcb_scenario()) -> Expected_Status :: term().
deduce_expected_status(#tcb_scenario{} = Scenario) ->
    #tcb_scenario{instance=_Inst, scenario_desc=Desc, initial_status=Init_Status, events=Events} = Scenario,
    Expected_Status = try   deduce(Desc, Init_Status, Events)
                      catch Error:Type -> error_logger:error_msg("Caught ~p error in ~p:deduce/3 ~p",
                                                                 [{Error, Type}, ?MODULE, erlang:get_stacktrace()]),
                                          failed
                      end,
    #tcb_test_case{scenario=Scenario, expected_status=Expected_Status}.

-spec vivify_scenario(Scenario :: tcb_scenario()) -> tcb_scenario_live_ref().
vivify_scenario(#tcb_scenario{} = _Scenario) ->
    uffda_registry_sup.

-spec transform_raw_scenario(pos_integer(), term()) -> tcb_scenario().
transform_raw_scenario(Idx, Raw_Scen) ->
    #tcb_scenario{instance = Idx, scenario_desc = Raw_Scen, initial_status = [], events = []}.

-spec translate_scenario_dsl(tcb_scenario_dsl_desc()) -> tcb_scenario_live_desc().
translate_scenario_dsl(Dsl_Scenario) when is_atom(Dsl_Scenario) ->
    Dsl_Scenario.

-spec translate_scenario_events(tcb_scenario_dsl_events()) -> tcb_scenario_live_events().
translate_scenario_events(Dsl_Events) when is_atom(Dsl_Events) ->
    Dsl_Events.

-spec generate_observation(tcb_scenario_live_ref(), tcb_test_case()) -> term().
generate_observation(_Live_Model_Ref, #tcb_test_case{} = _Test_Case_Instance) ->
    success.

-spec passed_test_case(Case_Number     :: pos_integer(),
                              Expected_Status :: tcb_scenario_dsl_status(),
                              Observed_Status :: tcb_scenario_live_status()) -> boolean().
passed_test_case(_Case_Number, _Expected_Status, _Observed_Status) ->
    true.

%%--------------------------------
%% Support functions
%%--------------------------------
deduce_event([Service], register) when is_atom(Service) -> {ok, [Service]};
deduce_event([Service], unregister) when is_atom(Service) -> {ok, []};
deduce_event(Service, which_services)  -> {Service, Service}. 
deduce(Desc, _Init_Status, Events) ->
    {_, Output} = lists:foldl(fun(Event, {RegisteredService, Results}) ->
                    {Return, NewlyRegistered} = deduce_event(RegisteredService, Event), 
                    {NewlyRegistered, [Return|Results]}
                    end, {[Desc], []}, Events), 
    lists:reverse(Output).
