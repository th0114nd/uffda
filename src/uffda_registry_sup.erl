-module(uffda_registry_sup).
-behavior(supervisor).

-export([
         start_link/0,
         init/1,
         start_child/3,
         stop_child/1,
         which_services/0
        ]).

-define(SERVER, ?MODULE).
-define(CHILD(__Mod, __Args), {__Mod, {__Mod, start_link, __Args},
                               permanent, 2000, worker, [__Mod]}).

-include("uffda.hrl").


%%-------------------------------------------------------------------
%% PUBLIC API
%%-------------------------------------------------------------------

-spec start_link() -> {ok, pid()}.
%% @doc
%%   Start the central Uffda Service Registry supervisor.
%%   It manages one {@link uffda_service_fsm} per registered service.
%% @end
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {}).


-spec start_child(service_name(), service_pid() | undefined, service_options())
                 -> {ok, service_fsm_pid()}
                        | {error, {already_started, service_fsm_pid()}}.
%% @doc
%%   Create a new {@link uffda_service_fsm} process for the Pid specified by
%%   Service pid, or waits for a pid if it is 'undefined'.
%% @end
start_child(Service_Name, Service_Pid, Options)
  when is_atom(Service_Name), (is_pid(Service_Pid) or (Service_Pid == undefined)), is_record(Options, service_options)  ->
    supervisor:start_child(?MODULE, [Service_Name, Service_Pid, Options]).

-spec stop_child(service_fsm_pid()) -> ok | {error, any()}.
%% @doc
%%   Stop a registered {@link uffda_service_fsm}. The service name
%%   will no longer be registered.
%% @end
stop_child(Pid)
  when is_pid(Pid) ->
    supervisor:terminate_child(?SERVER, Pid).

-spec which_services() -> [service_fsm_pid()].
%% @doc
%   Fetch a list of the registered {@link uffda_service_fsm} pids.
%% @end
which_services() ->
    [Pid || {undefined, Pid, worker, _Mod} <- supervisor:which_children(?SERVER)].


%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
%% @private
-spec init({}) -> {ok, {{supervisor:strategy(), non_neg_integer(), non_neg_integer()},
                        [supervisor:child_spec()]}}.
%% @doc
%%   Initialize the supervisor to manage simple_one_for_one
%%   {@link uffda_service_fsm} transient workers.
%% @end
init({}) ->
    Fsm_Spec = ?CHILD(uffda_service_fsm, []),
    {ok, {{simple_one_for_one, 5, 60}, [Fsm_Spec]}}.
