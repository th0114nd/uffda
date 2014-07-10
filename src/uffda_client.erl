%% @doc
%%   External applications use uffda to register and track the status
%%   of running services. A registered service is represented
%%   internally as a supervised FSM with the name
%%   <code>list_to_atom(Service_Name_String ++ "_uffda_service_fsm")</code>.
%%   The FSM maintains an internal model of the current availability
%%   of the service.
%%
%%   Service registry and status reporting are partially the
%%   responsibility of the Service Application writer. An example
%%   of use is shown in {@link thermomemeter}. In general, the service
%%   should register its name once (hopefully, but repeated registration
%%   of the service name has no impact if the FSM is already running),
%%   followed by calling starting_service/2 before the initialization
%%   sequence of the service begins. Once the service is available,
%%   a subsequent call to set_service_online/1 should be made. If the
%%   service is brought down in a controlled fashion, set_service_offline/1
%%   should be called just prior to initiating the shutdown sequence.
%%
%%   Uffda will monitor the service pid that is provided in the
%%   starting_service/2 call and record when the service has crashed
%%   or shutdown without notifying the service registry. This effort
%%   is transparent to the application. Restarts caused by a supervisor
%%   should follow the same startup sequence as the original service
%%   startup, thereby ensuring service_starting/2 is called with the
%%   new pid.
%%
%%   One benefit of using uffda is that it is possible to know if a
%%   service is starting up for the first time or if it has crashed
%%   and restarted because the registry FSM survives service failure.
%%   This knowledge may be useful to inform the restart strategy which
%%   may benefit from a different sequence than a cold start.
%%
%%   The function {@link service_status/1} indicates the
%%   possible states that the service registry may report.
%% @end
-module(uffda_client).

-export([
         register_service/1,
         register_service/2,
         unregister_service/1,
         starting_service/1,
         starting_service/2,
         set_service_online/1,
         set_service_offline/1,
         service_status/1,
         which_services/0
        ]).

-include("uffda.hrl").

-spec default_options() -> #service_options{}.
default_options() -> #service_options{stimeout = ?MAX_STARTUP_TIME}.

%% Register reserves a service name for future monitoring.
-spec register_service(service_name()) -> ok.
%% @doc
%%    Reserve a service name for future monitoring.
%%    This is accomplished internally by asking the registry
%%    supervisor to add a new {@link uffda_service_fsm} worker.
%%    If one already exists it is reported as the new registered
%%    service FSM.
%% @end
register_service(Service_Name)
  when is_atom(Service_Name) ->
    ct:log("Reg 1"),
    register_service(Service_Name, undefined, default_options()).

-spec register_service(service_name(), service_pid()) -> ok.
%% @doc
%%    Reserve a service name (see {@link register_service/1}) provide
%%    an already running pid as the service process to monitor.
%%    supervisor to add a new {@link uffda_service_fsm} worker.
%%    If one already exists it is reported as the new registered
%%    service FSM.
%% @end
register_service(Service_Name, Service_Pid)
  when is_atom(Service_Name), is_pid(Service_Pid) ->
    register_service(Service_Name, Service_Pid, default_options()).

register_service(Service_Name, undefined, Options)
  when is_atom(Service_Name), is_record(Options, service_options) ->
    ct:log("Reg 3"),
    _ = uffda_registry_sup:start_child(Service_Name, undefined, Options),
    ok;
register_service(Service_Name, Service_Pid, Options)
  when is_atom(Service_Name), is_pid(Service_Pid) or Service_Pid == undefined, is_record(Options, service_options)  ->
    case uffda_registry_sup:start_child(Service_Name, Service_Pid, Options) of
        {ok, _Fsm_Pid} -> ok;
        {error, {already_started, Fsm_Pid}} ->
            trigger_all_event(Fsm_Pid, {re_init, Service_Pid})
    end.

-spec unregister_service(service_name()) -> ok | {error, term()}.
%% @doc
%%    Remove a service name from the service registry.
%%    This is accomplished internally by asking the registry
%%    supervisor to stop the {@link uffda_service_fsm} worker.
%% @end
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

-spec which_services() -> [service_name()].
%% @doc
%%   Return a list of all the services that are currently registered,
%%   regardless of their current status.
%% @end
which_services() ->
    FSM_Pids = which_service_fsms(),
    [gen_fsm:sync_send_all_state_event(FSM, get_service_name) || FSM <- FSM_Pids].

-spec which_service_fsms() -> [service_fsm_pid()].
%% @private
%% @doc
%%   Return a list of all the service_fsm pids that are currently registered,
%%   regardless of their current status.
%% @end
which_service_fsms() ->
    uffda_registry_sup:which_services().


%% Service events cause the Service FSM to change the service status.
-type event_response()  :: {error, {not_registered, service_name()}} | ok.
-type status_response() :: {error, {not_registered, service_name()}} | service_status().


-spec starting_service(service_name()) -> event_response().
%% @doc
%% starting_service/2 with self as the second argument.
%% @end
starting_service(Service_Name) -> starting_service(Service_Name, self()).


-spec starting_service(service_name(), service_pid()) -> event_response().
%% @doc
%%   Indicating that a pid is the key process for a service and it is now
%%   beginning its initialization sequence. The service registry will allow
%%   it to be in a starting_up or restarting state for a limited time
%%   (default is 30 seconds) before automatically switching it to indicate
%%   the initialization is taking too long. To prevent this, you must call
%%   {@link set_service_online/1} or {@link set_service_offline/2} before the
%%   the timeout for initialization occurs.
%% @end
starting_service(Service_Name, Service_Pid) -> trigger_event(Service_Name, Service_Pid, starting).

-spec set_service_online(service_name()) -> event_response().
%% @doc
%%   Indicate that a service is now available for requests.
%% @end
set_service_online(Service_Name) -> trigger_event(Service_Name, online).

-spec set_service_offline (service_name()) -> event_response().
%% @doc
%%   Indicate that a service is about to be shutdown in a controlled
%%   manner and should be listed as a no longer available.
%% @end
set_service_offline(Service_Name) -> trigger_event(Service_Name, offline).

-spec service_status(service_name()) -> status_response().
%% @doc
%%   Report the current status of a service. Valid responses are:
%%
%%   <table border='1'>
%%   <tr><th>Status</th><th>Meaning</th></tr>
%%   <tr><td>registered</td>
%%       <td>The service name is allocated but is not currently bound to a pid.</td></tr>
%%   <tr><td>starting_up</td>
%%       <td>The service is running initialization for a cold start.</td></tr>
%%   <tr><td>slow_start</td>
%%       <td>The service has been initializing from a cold start for too long.</td></tr>
%%   <tr><td>restarting</td>
%%       <td>The service is running initialization for a restart.</td></tr>
%%   <tr><td>slow_restart</td>
%%       <td>The service has been initializing for a restart for too long.</td></tr>
%%   <tr><td>crashed</td>
%%       <td>The service pid stopped with a reason other than <code>'normal'</code>.</td></tr>
%%   <tr><td>down</td>
%%       <td>The service is not running.</td></tr>
%%   <tr><td>up</td>
%%       <td>The service is running.</td></tr>
%%   </table>
%% @end
service_status(Service_Name) -> trigger_all_event(Service_Name, current_status).

%% Internal support functions
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
