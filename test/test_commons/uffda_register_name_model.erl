%% @doc
%%   The register_name model tests only that a series of names can be
%%   registered/unregistered and the final set of registered names
%%   will be accurately reported by the Uffda service registry.
%% @end
-module(uffda_register_name_model).
-behaviour(scenev).

-export([
         retrieve_sources/0,
         deduce_expected/1,
         transform_raw_scenario/2,
         translate_dsl/1,
         translate_events/1,
         generate_observation/1,
         passed_test_case/3
        ]).

-include_lib("test_commons/include/scenev.hrl").

-define(EVENTS, [which_services, register, which_services, unregister, which_services]).

-spec retrieve_sources() -> [{Model_Id :: scenev_model_id(), Source :: scenev_source()}].
%% @doc
%%   Test models are stored as static data files containing atom names to be
%%   registered as an Uffda Service Name. Each test model is stored in a file
%%   named after its model id.
%% @end
retrieve_sources() ->
    Dir = code:priv_dir(uffda) ++ "/register_name_models/",
    [{filename:rootname(Dir), {dir, Dir}}].

-spec transform_raw_scenario(pos_integer(), atom()) -> {single, scenev_scenario()}.
%% @doc
%%   Raw scenarios are created by reading the corresponding test model file.
%%   All register_model names are atoms. They are converted to scenev_scenario()
%%   instances by making the scenario description be the Service Name.
%% @end
transform_raw_scenario(Scenario_Number, Service_Name)
  when is_atom(Service_Name), is_integer(Scenario_Number), Scenario_Number > 0 ->
    {single, #scenev_scenario{instance = Scenario_Number, scenario_desc = Service_Name,
                        initial_status = [], events = ?EVENTS}}.

-spec deduce_expected(Scenario_Instance :: scenev_scenario()) -> Expected_Status :: term().
%% @doc
%%   Given a scenario instance containing a Service_Name and a set of Events,
%%   Determine what the final status of the service registry will be by adding
%%   the given Service_Name for each register event and removing it for each
%%   unregister event.
%% @end
deduce_expected(#scenev_scenario{scenario_desc=Desc, events=Events} = _Scenario) ->
    {_, Output}
        = lists:foldl(fun(Event, {RegisteredService, Results}) ->
                              case Event of
                                  register       -> {[Desc],            [ok                | Results]};
                                  unregister     -> {[],                [ok                | Results]};
                                  which_services -> {RegisteredService, [RegisteredService | Results]}
                              end
                      end, {[], []}, Events), 
    lists:reverse(Output).
    
-spec translate_dsl(scenev_dsl_desc()) -> scenev_live_desc().
%% @doc
%%   No Domain Specific Language (DSL) is needed for this test, so the existing
%%   Service_Name in the scenario is returned unchanged as the translated name.
%% @end
translate_dsl(Name) 
  when is_atom(Name) ->
    Name.

-spec translate_events(scenev_dsl_events()) -> scenev_live_events().
%% @doc
%%   The events are atoms that indicate the uffda_client function to run when
%%   the event occurs. A list of events is translated to a list of function
%%   closures that are wrapped to prevent execution until the observation phase
%%   of testing.
%% @end
translate_events(Events)
  when is_list(Events) ->
    fun(Name) -> [call_uffda_client(Name, Event) || Event <- Events] end.

call_uffda_client( Service_Name, register)       -> uffda_client:register_service   (Service_Name);
call_uffda_client( Service_Name, unregister)     -> uffda_client:unregister_service (Service_Name);
call_uffda_client(_Service_Name, which_services) -> uffda_client:which_services     ().

-spec generate_observation(scenev_scenario()) -> term().
%% @doc
%%   The system is observed by executing the translated event closures in serial
%%   order to capture each return value and to place the Uffda service registry
%%   into the final state from which system status can be read.
%% @end
generate_observation(#scenev_scenario{scenario_desc = Service_Name, events = Events} = _Scenario) ->
    [call_uffda_client(Service_Name, Event) || Event <- Events].

-spec passed_test_case(Case_Number     :: pos_integer(),
                       scenev_expected_status(),
                       scenev_observed_status()) -> boolean().
%% @doc
%%   A test case passes when each of the return values match after each event
%%   executes. The expected and observered lists are arranged to be time-ordered
%%   lists of return values from the service registry, with the final value
%%   being the final status of the service registry.
%% @end
passed_test_case(_Case_Number, Expected_Status, Observed_Status) ->
    Expected_Status =:= Observed_Status.

%%--------------------------------
%% Support functions
%%--------------------------------
