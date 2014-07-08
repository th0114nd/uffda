-module(uffda_client).

-export([
         register_service/1,
         register_service/2,
         reset_service/1,
         unregister_service/1,
         starting_service/1,
         set_service_online/1,
         set_service_offline/1,
         service_status/1,
         which_service_pids/0,
         which_service_names/0
        ]).

-include("uffda.hrl").

-spec which_service_pids() -> [service_pid()].
which_service_pids() ->
    _FSM_Pids = uffda_registry_sup:which_children().

-spec which_service_names() -> [service_name()].
which_service_names() ->
    FSM_Pids = which_service_pids(),
    [gen_fsm:sync_send_all_state_event(Pid, get_service_name) || Pid <- FSM_Pids].

%% Register reserves a service name for future monitoring.
-spec register_service   (service_name())                -> ok.
-spec register_service   (service_name(), service_pid()) -> ok.
-spec unregister_service (service_name()) -> ok.

register_service(Service_Name)
  when is_atom(Service_Name) ->
    register_service(Service_Name, self()).

register_service(Service_Name, Service_Pid)
  when is_atom(Service_Name), is_pid(Service_Pid) ->
    case uffda_registry_sup:start_child(Service_Name, Service_Pid) of
        {ok, _Fsm_Pid} -> ok;
        {error, {already_started, Fsm_Pid}} ->
            gen_fsm:sync_send_all_state_event(Fsm_Pid, {re_init, Service_Pid})
    end.

unregister_service(Service_Name)
  when is_atom(Service_Name) ->
    Fsm_Name = uffda_service_fsm:fsm_name_from_service(Service_Name),
    case whereis(Fsm_Name) of
        undefined -> ok;
        Fsm_Pid -> uffda_registry_sup:stop_child(Fsm_Pid)
    end.


%% Service events cause the Service FSM to change the service status.
-type event_response()  :: {error, {not_registered, service_name()}} | ok.
-type status_response() :: {error, {not_registered, service_name()}} | service_status().

-spec reset_service       (service_name()) -> event_response().
-spec starting_service    (service_name()) -> event_response().
-spec set_service_online  (service_name()) -> event_response().
-spec set_service_offline (service_name()) -> event_response().
-spec service_status      (service_name()) -> status_response().
    
starting_service    (Service_Name) -> trigger_event(Service_Name, starting,       async).
set_service_online  (Service_Name) -> trigger_event(Service_Name, online,         async).
set_service_offline (Service_Name) -> trigger_event(Service_Name, offline,        async).
reset_service       (Service_Name) -> trigger_event(Service_Name, reset,          async).
service_status      (Service_Name) -> trigger_event(Service_Name, current_status, sync).

trigger_event(Service_Name, Service_Event, Sync_Or_Async)
  when is_atom(Service_Name) ->
    Fsm_Name = uffda_service_fsm:fsm_name_from_service(Service_Name),
    case Sync_Or_Async of
        sync  -> gen_fsm:sync_send_all_state_event(Fsm_Name, Service_Event);
        async -> gen_fsm:send_event(Fsm_Name, Service_Event)
    end.
