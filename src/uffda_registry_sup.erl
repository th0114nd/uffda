-module(uffda_registry_sup).
-behavior(supervisor).

-export([start_link/0, init/1, start_child/2]).

-define(SERVER, ?MODULE).
-define(CHILD(__Mod, __Args), {__Mod, {__Mod, start_link, __Args},
                               permanent, 2000, worker, [__Mod]}).

%%-------------------------------------------------------------------
%% PUBLIC API
%%-------------------------------------------------------------------

%% @doc Starts the supervisor.
-spec start_link() -> {ok, pid()}.

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {}).

start_child(Service, Service_Pid) ->
    supervisor:start_child(?MODULE, [Service, Service_Pid]).

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
%% @private
-spec init({}) -> {ok, {{supervisor:strategy(), non_neg_integer(), non_neg_integer()},
                        [supervisor:child_spec()]}}.
init({}) -> 
    Fsm_Spec = ?CHILD(uffda_service_fsm, []),
    {ok, {{simple_one_for_one, 5, 60}, [Fsm_Spec]}}.
