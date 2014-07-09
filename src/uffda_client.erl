-module(uffda_client).

-export([
         register_service/1,
         register_service/2,
         unregister_service/1,
         starting_service/2,
         set_service_online/1,
         set_service_offline/1,
         service_status/1,
%        which_service_pids/0,
         which_service_names/0
        ]).

-include("uffda.hrl").

%<<<<<<< HEAD
%-spec which_service_pids() -> [service_pid()].
%which_service_pids() ->
%    _FSM_Pids = uffda_registry_sup:which_children().
%=======
%
%-spec which_services() -> [service_pid()].
%which_services() ->
%    uffda_registry_sup:which_services().
%>>>>>>> jaynel-proper

-spec which_service_names() -> [service_name()].
which_service_names() ->
    FSM_Pids = uffda_registry_sup:which_services(),
    [gen_fsm:sync_send_all_state_event(Pid, get_service_name) || Pid <- FSM_Pids].

%% Register reserves a service name for future monitoring.
-spec register_service(service_name()) -> ok.
register_service(Service_Name)
  when is_atom(Service_Name) ->
    _ = uffda_registry_sup:start_child(Service_Name),
    ok.

-spec register_service   (service_name(), service_pid()) -> ok.
register_service(Service_Name, Service_Pid)
  when is_atom(Service_Name), is_pid(Service_Pid) ->
    case uffda_registry_sup:start_child(Service_Name, Service_Pid) of
        {ok, _Fsm_Pid} -> ok;
        {error, {already_started, Fsm_Pid}} ->
            trigger_all_event(Fsm_Pid, {re_init, Service_Pid})
    end.

-spec unregister_service(service_name()) -> ok | {error, term()}.
unregister_service(Service_Name)
  when is_atom(Service_Name) ->
    case get_registered_fsm(Service_Name) of
        Fsm_Pid when is_pid(Fsm_Pid) ->
            uffda_registry_sup:stop_child(Fsm_Pid);
        Error -> Error
    end.

get_registered_fsm(Service_Name) ->
    case uffda_service_fsm:existing_fsm_name_from_service(Service_Name) of
        Fsm_Name when is_atom(Fsm_Name) ->
            case whereis(Fsm_Name) of
                undefined -> {error, {not_registered, Service_Name}};
                Fsm_Pid   -> Fsm_Pid
            end;
        Error -> Error
    end.


%% Service events cause the Service FSM to change the service status.
-type event_response()  :: {error, {not_registered, service_name()}} | ok.
-type status_response() :: {error, {not_registered, service_name()}} | service_status().

-spec starting_service(service_name(), service_pid()) -> event_response().
%%% @doc starting_service
starting_service(Service_Name, Service_Pid) -> trigger_event(Service_Name, Service_Pid, starting).

-spec set_service_online(service_name()) -> event_response().
%%% @doc set_service_online
set_service_online(Service_Name) -> trigger_event(Service_Name, online).

-spec set_service_offline (service_name()) -> event_response().
%%% @doc set_service_offline
set_service_offline(Service_Name) -> trigger_event(Service_Name, offline).

-spec service_status(service_name()) -> status_response().
%%% @doc service_status
service_status(Service_Name) -> trigger_all_event(Service_Name, current_status).

trigger_event(Service_Name, Service_Pid, Service_Event)
  when is_atom(Service_Name), is_pid(Service_Pid) ->
    case get_registered_fsm(Service_Name) of
        Fsm_Pid when is_pid(Fsm_Pid) ->
            gen_fsm:send_event(Fsm_Pid, {Service_Event, Service_Pid});
        Error -> Error
    end.
          
    
trigger_event(Service_Name, Service_Event) when is_atom(Service_Name) ->
    case get_registered_fsm(Service_Name) of
        Fsm_Pid when is_pid(Fsm_Pid) ->
            gen_fsm:send_event(Fsm_Pid, Service_Event);
        Error -> Error
    end.

trigger_all_event(Service_Pid,  Service_Event) when is_pid(Service_Pid) ->
    gen_fsm:sync_send_all_state_event(Service_Pid, Service_Event);
trigger_all_event(Service_Name, Service_Event) when is_atom(Service_Name) ->
    case get_registered_fsm(Service_Name) of
        Fsm_Pid when is_pid(Fsm_Pid) ->
            gen_fsm:sync_send_all_state_event(Fsm_Pid, Service_Event);
        Error -> Error
    end.
