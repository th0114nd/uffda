-module(uffda_registry_sup).
-behavior(supervisor).

-export([start_link/0,
         init/1,
         start_child/1,
         start_child/2,
         stop_child/1,
         which_children/0]).

-define(SERVER, ?MODULE).
-define(CHILD(__Mod, __Args), {__Mod, {__Mod, start_link, __Args},
                               permanent, 2000, worker, [__Mod]}).

-include("uffda.hrl").


%%-------------------------------------------------------------------
%% PUBLIC API
%%-------------------------------------------------------------------

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {}).

-spec start_child(service_name())
                 -> {ok, service_fsm_pid()}
                        | {error, {already_started, service_fsm_pid()}}.
start_child(Service_Name) 
  when is_atom(Service_Name) ->
    supervisor:start_child(?MODULE, [Service_Name]).

-spec start_child(service_name(), service_pid())
                 -> {ok, service_fsm_pid()}
                        | {error, {already_started, service_fsm_pid()}}.
start_child(Service_Name, Service_Pid) 
  when is_atom(Service_Name), is_pid(Service_Pid) ->
    supervisor:start_child(?MODULE, [Service_Name, Service_Pid]).

-spec stop_child(service_fsm_pid()) -> ok | {error, any()}.
stop_child(Pid)
  when is_pid(Pid) ->
    supervisor:terminate_child(?SERVER, Pid).

-spec which_children() -> [service_fsm_pid()].
which_children() ->
    [Pid || {undefined, Pid, worker, _Mod} <- supervisor:which_children(?SERVER)].


%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
%% @private
-spec init({}) -> {ok, {{supervisor:strategy(), non_neg_integer(), non_neg_integer()},
                        [supervisor:child_spec()]}}.
init({}) -> 
    Fsm_Spec = ?CHILD(uffda_service_fsm, []),
    {ok, {{simple_one_for_one, 5, 60}, [Fsm_Spec]}}.
