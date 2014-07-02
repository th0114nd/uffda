-module(sr_client).

-export([register_me/1, get_state/1, go_up/1, go_down/1, reset/1, wait/1]).
-export([service_loop/1]).

-include("uffda.hrl").

-spec register_me(service_name()) -> {ok, pid()} | {error, term()}.
register_me(Service) ->
    %% start_child is a simple_one_for_one so args are passed as a list
    case service_registry_sup:start_child(service_registry_sup, [Service, self()]) of
        {ok, _} -> ok;
        {error, {already_started, ServiceFSM}} -> re_register_me(ServiceFSM, self())
    end.

-spec re_register_me(atom(), pid()) -> {ok, pid()}. 
re_register_me(ServiceFSM, ServicePid) ->
    gen_fsm:sync_send_all_state_event(ServiceFSM, {re_init, ServicePid}).

-spec get_state(service_name()) -> atom().
get_state(Service) ->
    FSM = service_fsm:fsm_name_from_service(Service),
    gen_fsm:sync_send_all_state_event(FSM, get_state).

-type response() :: ok | {error, term()}.

-spec go_up(service_name()) -> response().
go_up(Service) ->
    FSM = service_fsm:fsm_name_from_service(Service),
    gen_fsm:send_event(FSM, go_up).

-spec go_down(service_name()) -> response().
go_down(Service) ->
    FSM = service_fsm:fsm_name_from_service(Service),
    gen_fsm:send_event(FSM, go_down).

-spec reset(service_name()) -> response().
reset(Service) ->
    FSM = service_fsm:fsm_name_from_service(Service),
    gen_fsm:send_event(FSM, reset).

-spec wait(service_name()) -> response().
wait(Service) ->
    FSM = service_fsm:fsm_name_from_service(Service),
    gen_fsm:send_event(FSM, wait).

-spec service_loop(atom()) -> term().
service_loop(Name) ->
    _ = receive
            die -> exit(kill);
            reg -> sr_client:register_me(Name);
            up -> sr_client:go_up(Name);
            down -> sr_client:go_down(Name);
            reset -> sr_client:reset(Name);
            {state, Pid} -> Pid ! sr_client:get_state(Name)
        end,
    service_loop(Name).
