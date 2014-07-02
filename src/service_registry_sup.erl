-module(service_registry_sup).
-behavior(supervisor).

-export([start_link/0, init/1]).

-define(CHILD(__Name, __Mod, __Args), {__Name, 
                                       {__Mod, start_link, __Args},
                                       permanent, 
                                       2000, 
                                       worker,
                                       [__Mod]}).
%%-------------------------------------------------------------------
%% PUBLIC API
%%-------------------------------------------------------------------

%% @doc Starts the supervisor.
-spec start_link() -> {ok, pid()}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {}).


%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
%% @private
-spec init({}) -> {ok, {{supervisor:strategy(), non_neg_integer(), non_neg_integer()},
                        [supervisor:child_spec()]}}.
init({}) -> 
    FSM_Spec = ?CHILD(service_fsm, service_fsm, []),
    {ok, {{simple_one_for_one, 5, 60}, [FSM_Spec]}}.
