-module(service_registry_sup).
-behavior(supervisor).

-export([start_link/0, init/1]).

-include_lib("erlang_commons/include/supervisor_macros.hrl").
-include_lib("erlang_commons/include/tts_sup_types.hrl").

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
-spec init({}) -> sup_init_return(). 

init({}) -> 
    FSM_Spec = ?CHILD(service_fsm, service_fsm, []),
    {ok, {{simple_one_for_one, 5, 60}, [FSM_Spec]}}.
