-module(uffda_register_name_model).
-behaviour(tc_proper_model).

-export([
         get_all_test_model_ids/0,
         generate_proper_model/2,
         deduce_proper_expected_status/1,

         vivify_proper_scenario/1,
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

-spec generate_proper_model(Model_Id :: tc_proper_model_id(), Source :: tc_proper_model_source()) -> tc_proper_model().
generate_proper_model(Id, {file, Filename} = Source) ->
    {ok, Scenarios} = file:consult(Filename),
    Pairs = lists:zip(lists:seq(1, length(Scenarios)), Scenarios),
    TCScenarios = [#tc_proper_scenario{instance = Idx, 
                                       scenario_desc = Name, 
                                       initial_status = [], 
                                       events = ?EVENTS} || {Idx, Name} <- Pairs],
    #tc_proper_model{id=Id, source=Source, behaviour=?MODULE, scenarios=TCScenarios}.

-spec deduce_proper_expected_status(Scenario_Instance :: tc_proper_scenario()) -> Expected_Status :: term().
deduce_proper_expected_status(#tc_proper_scenario{} = Scenario) ->
    #tc_proper_scenario{instance=_Inst, scenario_desc=Desc, initial_status=Init_Status, events=Events} = Scenario,
    Expected_Status = try   deduce(Desc, Init_Status, Events)
                      catch Error:Type -> error_logger:error_msg("Caught ~p error in ~p:deduce/3 ~p",
                                                                 [{Error, Type}, ?MODULE, erlang:get_stacktrace()]),
                                          failed
                      end,
    #tc_proper_test_case{scenario=Scenario, expected_status=Expected_Status}.

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

deduce_loop(_Name, [], _Registered) -> [].
deduce_loop(Name, [E | ES], Registered) ->
    case E of
        register -> [case ordsets:member(Name, Registered) of
                         false -> NewReg = ordsets:add_element(Name, Registered),
                                  ok;
                         true -> NewReg = Reg,
                                 {error, already_started}
                    end
                    | deduce_loop(Name, ES, NewReg)];
        unregister -> [case ordsets:member(Name, Registered) of
                         true -> NewReg = ordsets:del_element(Name, Registered),
                                 ok;
                          false -> NewReg = Reg,
                                   {error, reason}
                       end
                       | deduce_loop(Name, ES, NewReg)];
        which_services -> [ordsets:to_list(Registered) | deduce_loop(Name, ES, Registered)]
    end.
