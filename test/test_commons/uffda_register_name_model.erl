-module(uffda_register_name_model).
-behaviour(tc_proper_model).

-export([
         get_all_test_model_ids/0,
         deduce_proper_expected_status/1,

         vivify_proper_scenario/1,
         transform_raw_scenario/2,
         translate_proper_scenario_dsl/1,
         translate_proper_scenario_events/1,
         generate_proper_observation/2,
         passed_proper_test_case/3
        ]).

-include("tc_proper_model.hrl").

-define(EVENTS, [which_services, register, which_services, unregister, which_services]).

-spec get_all_test_model_ids() -> [{Model_Id :: tc_proper_model_id(), Source :: tc_proper_model_source()}].
get_all_test_model_ids() ->
    Dir = "./priv/register_name_model/",
    {ok, Files} = file:list_dir(Dir),
    Pairs = [{filename:rootname(File), filename:absname(Dir ++ File)} || File <- Files],
    [{list_to_atom(Test_Name), {file, Abs_Path}} || {Test_Name, Abs_Path} <- Pairs].

-spec transform_raw_scenario(pos_integer(), atom()) -> tc_proper_scenario().
transform_raw_scenario(Idx, Name)
  when is_atom(Name), is_integer(Idx), Idx > 0 ->
    #tc_proper_scenario{instance = Idx, scenario_desc = Name, initial_status = [], events = ?EVENTS}.

-spec deduce_proper_expected_status(Scenario_Instance :: tc_proper_scenario()) -> Expected_Status :: term().
deduce_proper_expected_status(#tc_proper_scenario{} = Scenario) ->
    #tc_proper_scenario{instance=_Inst, scenario_desc=Desc, initial_status=Init_Status, events=Events} = Scenario,
    try   deduce(Desc, Init_Status, Events)
    catch Error:Type -> error_logger:error_msg("Caught ~p error in ~p:deduce/3 ~p",
                                               [{Error, Type}, ?MODULE, erlang:get_stacktrace()]),
                        failed
    end.
    

-spec vivify_proper_scenario(Scenario :: tc_proper_scenario()) -> tc_proper_scenario_live_ref().
vivify_proper_scenario(#tc_proper_scenario{} = _Scenario) ->
    uffda_registry_sup.

-spec translate_proper_scenario_dsl(tc_proper_scenario_dsl_desc()) -> tc_proper_scenario_live_desc().
translate_proper_scenario_dsl(Name) 
  when is_atom(Name) ->
    Name.

-spec translate_proper_scenario_events(tc_proper_scenario_dsl_events()) -> tc_proper_scenario_live_events().
translate_proper_scenario_events(Events)
  when is_list(Events) ->
    fun(Name) -> [call_uffda_client(Name, Event) || Event <- Events] end.

call_uffda_client(Name, register) -> uffda_client:register_service(Name);
call_uffda_client(Name, unregister) -> uffda_client:unregister_service(Name);
call_uffda_client(_Name, which_services) -> uffda_client:which_services().

-spec generate_proper_observation(tc_proper_scenario_live_ref(), tc_proper_test_case()) -> term().
generate_proper_observation(_Live_Model_Ref, #tc_proper_test_case{} = _Test_Case_Instance) ->
    success.

-spec passed_proper_test_case(Case_Number     :: pos_integer(),
                              Expected_Status :: tc_proper_scenario_dsl_status(),
                              Observed_Status :: tc_proper_scenario_live_status()) -> boolean().
passed_proper_test_case(_Case_Number, Expected_Status, Observed_Status) ->
    Expected_Status =:= Observed_Status.


%%--------------------------------
%% Support functions
%%--------------------------------
deduce(Name, [], Events = ?EVENTS) ->
    deduce_loop(Name, Events, ordsets:new()).

deduce_loop(_Name, [], _Reg) -> [];
deduce_loop(Name, [E | ES], Reg) ->
    case E of
        register -> {NewReg, Expected} = case ordsets:is_element(Name, Reg) of
                         false -> {ordsets:add_element(Name, Reg), ok};
                         true -> {Reg, {error, already_started}}
                    end,
                    [Expected | deduce_loop(Name, ES, NewReg)];
        unregister -> {NewReg, Expected} = case ordsets:is_element(Name, Reg) of
                          true -> {ordsets:del_element(Name, Reg), ok};
                          false -> {Reg, {error, reason}}
                       end,
                       [Expected | deduce_loop(Name, ES, NewReg)];
        which_services -> [ordsets:to_list(Reg) | deduce_loop(Name, ES, Reg)]
    end.