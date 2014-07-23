%% @doc
%%   The register_name model tests only that a series of names can be
%%   registered/unregistered and the final set of registered names
%%   will be accurately reported by the Uffda service registry.
%% @end
-module(uffda_register_name_model).
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

-define(EVENTS, [which_services, register, which_services, unregister, which_services]).

-spec get_all_test_model_ids() -> [{Model_Id :: tcb_model_id(), Source :: tcb_model_source()}].
%% @doc
%%   Test models are stored as static data files containing atom names to be
%%   registered as an Uffda Service Name. Each test model is stored in a file
%%   named after its model id.
%% @end
get_all_test_model_ids() ->
    Dir = "./priv/register_name_models/",
    {ok, Files} = file:list_dir(Dir),
    Pairs = [{filename:rootname(File), filename:absname(Dir ++ File)} || File <- Files],
    [{list_to_atom(Test_Name), {file, Abs_Path}} || {Test_Name, Abs_Path} <- Pairs].

-spec transform_raw_scenario(pos_integer(), atom()) -> tcb_scenario().
%% @doc
%%   Raw scenarios are created by reading the corresponding test model file.
%%   All register_model names are atoms. They are converted to tcb_scenario()
%%   instances by making the scenario description be the Service Name.
%% @end
transform_raw_scenario(Scenario_Number, Service_Name)
  when is_atom(Service_Name), is_integer(Scenario_Number), Scenario_Number > 0 ->
    #tcb_scenario{instance = Scenario_Number, scenario_desc = Service_Name,
                        initial_status = [], events = ?EVENTS}.

-spec deduce_expected_status(Scenario_Instance :: tcb_scenario()) -> Expected_Status :: term().
%% @doc
%%   Given a scenario instance containing a Service_Name and a set of Events,
%%   Determine what the final status of the service registry will be by adding
%%   the given Service_Name for each register event and removing it for each
%%   unregister event.
%% @end
deduce_expected_status(#tcb_scenario{} = Scenario) ->
    #tcb_scenario{instance=_Inst, scenario_desc=Desc, initial_status=Init_Status, events=Events} = Scenario,
    try   deduce(Desc, Init_Status, Events)
    catch Error:Type -> error_logger:error_msg("Caught ~p error in ~p:deduce/3 ~p",
                                               [{Error, Type}, ?MODULE, erlang:get_stacktrace()]),
                        failed
    end.
    
-spec vivify_scenario(Scenario :: tcb_scenario()) -> tcb_scenario_live_ref().
%% @doc
%%   No live scenario elements need to be set up since the Uffda service registry
%%   is maintained by a single registry supervisor. This function just returns the
%%   name of the uffda_registry_sup.
%% @end
vivify_scenario(#tcb_scenario{} = _Scenario) ->
    uffda_registry_sup.

-spec translate_scenario_dsl(tcb_scenario_dsl_desc()) -> tcb_scenario_live_desc().
%% @doc
%%   No Domain Specific Language (DSL) is needed for this test, so the existing
%%   Service_Name in the scenario is returned unchanged as the translated name.
%% @end
translate_scenario_dsl(Name) 
  when is_atom(Name) ->
    Name.

-spec translate_scenario_events(tcb_scenario_dsl_events()) -> tcb_scenario_live_events().
%% @doc
%%   The events are atoms that indicate the uffda_client function to run when
%%   the event occurs. A list of events is translated to a list of function
%%   closures that are wrapped to prevent execution until the observation phase
%%   of testing.
%% @end
translate_scenario_events(Events)
  when is_list(Events) ->
    fun(Name) -> [call_uffda_client(Name, Event) || Event <- Events] end.

call_uffda_client( Service_Name, register)       -> uffda_client:register_service   (Service_Name);
call_uffda_client( Service_Name, unregister)     -> uffda_client:unregister_service (Service_Name);
call_uffda_client(_Service_Name, which_services) -> uffda_client:which_services     ().

-spec generate_observation(tcb_scenario_live_ref(), tcb_test_case()) -> term().
%% @doc
%%   The system is observed by executing the translated event closures in serial
%%   order to capture each return value and to place the Uffda service registry
%%   into the final state from which system status can be read.
%% @end
generate_observation(_Live_Model_Ref, #tcb_test_case{scenario=Scenario} = _Test_Case_Instance) ->
    #tcb_scenario{scenario_desc=Service_Name, events=Events} = Scenario,
    [call_uffda_client(Service_Name, Event) || Event <- Events].

-spec passed_test_case(Case_Number     :: pos_integer(),
                              Expected_Status :: tcb_scenario_dsl_status(),
                              Observed_Status :: tcb_scenario_live_status()) -> boolean().
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
deduce(Desc, _Init_Status, Events) ->
    {_, Output}
        = lists:foldl(fun(Event, {RegisteredService, Results}) ->
                              case Event of
                                  register       -> {[Desc],            [ok                | Results]};
                                  unregister     -> {[],                [ok                | Results]};
                                  which_services -> {RegisteredService, [RegisteredService | Results]}
                              end
                      end, {[], []}, Events), 
    lists:reverse(Output).
