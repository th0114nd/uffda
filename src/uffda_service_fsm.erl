-module(uffda_service_fsm).
-behavior(gen_fsm).

-export([init/1, handle_event/3, handle_sync_event/4,
		 handle_info/3, code_change/4, terminate/3]).

-export([start_link/2, fsm_name_from_service/1]).

-record(state_data, {
          name        :: service_name(),
          pid         :: service_pid(),
          monitor_ref :: reference()
         }).

-type state_data()     :: #state_data{}.
-type fsm_state_name() :: atom().
-type reason() :: atom().

%% States of a Service
-export(['STARTING_UP'/2,
         'UP'/2,
         'DOWN'/2]).

-include("uffda.hrl").

-spec fsm_name_from_service(service_name()) -> service_fsm().
fsm_name_from_service(Service) ->
    list_to_atom(atom_to_list(Service) ++ "_fsm").

-spec start_link(service_name(), service_pid()) -> {ok, service_fsm_pid()}.
start_link(Service, ServicePid) ->
    ServFSM = fsm_name_from_service(Service),
    gen_fsm:start_link({local, ServFSM}, ?MODULE, {Service, ServicePid}, []).


%%----------------------------------------------------
%% States of a Service
%%----------------------------------------------------

%% A Service is starting up
-spec 'STARTING_UP'(term(), state_data()) -> {next_state, fsm_state_name(), state_data()}.
'STARTING_UP'(go_up, StateData) -> {next_state, 'UP', StateData};
'STARTING_UP'(wait, StateData) -> {next_state, 'DOWN', StateData};
'STARTING_UP'(Event, StateData) ->
    Err_Msg = "~p: unexpected event \"~p\", state was ~p",
    Err_Args = [?MODULE, Event, StateData],
	error_logger:error_msg(Err_Msg, Err_Args),
    {next_state, 'STARTING_UP', StateData}.

%% A Service is up and running
-spec 'UP'(term(), state_data()) -> {next_state, fsm_state_name(), state_data()}.
'UP'(go_down, StateData) -> {next_state, 'DOWN', StateData};
'UP'(Event, StateData) ->
    Err_Msg = "~p: unexpected event \"~p\", state was \"~p\" state data was \"~p\"",
    Err_Args = [?MODULE, Event, 'UP', StateData],
	error_logger:error_msg(Err_Msg, Err_Args),
	{next_state, 'UP', StateData}.

%% A Service is down
-spec 'DOWN'(term(), state_data()) -> {next_state, fsm_state_name(), state_data()}.
'DOWN'(reset, StateData) -> {next_state, 'STARTING_UP', StateData};
'DOWN'(Event, StateData) ->
    Err_Msg = "~p: unexpected \"~p\", state was \"~p\", state data was \"~p\"",
    Err_Args = [?MODULE, Event, 'DOWN', StateData],
    error_logger:error_msg(Err_Msg, Err_Args),
    {next_state, 'DOWN', StateData}.

%%---------------------------------------------------
%% gen_fsm API callbacks 
%%---------------------------------------------------

-spec init({service_name(), service_pid()}) -> {ok, fsm_state_name(), state_data()}.
init({Service, Service_Pid}) ->
    {ok, 'STARTING_UP', reinitialize(Service, Service_Pid, #state_data{})}.

-spec reinitialize(service_name(), service_pid(), state_data()) -> state_data().
reinitialize(Service, ServicePid, #state_data{} = OldStateData) ->
    MonRef = erlang:monitor(process, ServicePid),
    OldStateData#state_data{name = Service, pid = ServicePid, monitor_ref = MonRef}.

-spec handle_event(term(), fsm_state_name(), state_data())
                  -> {next_state, fsm_state_name(), state_data()}
                         | {stop, fsm_state_name(), state_data()}.
handle_event(Event, StateName, StateData) ->
    Err_Msg = "~p: unexpected event \"~p\", state data was ~p",
    Err_Args = [?MODULE, Event, StateData],
    error_logger:error_msg(Err_Msg, Err_Args),
    {next_state, StateName, StateData}.

-spec handle_sync_event(term(), {pid(), term()}, fsm_state_name(), state_data())
                       -> {reply, term(), fsm_state_name(), state_data()}.
handle_sync_event({re_init, ServicePid}, 
                  _From, _StateName, 
                  #state_data{name=Name} = StateData) ->
    {reply, ok, 'STARTING_UP', reinitialize(Name, ServicePid, StateData)};
handle_sync_event(get_state, _From, StateName, StateData) ->
    {reply, StateName, StateName, StateData};
handle_sync_event(Event, _From, StateName, StateData) ->
    Err_Msg = "~p:unexpected event \"~p\", state data was ~p",
    Err_Args = [?MODULE, Event, StateData],
    error_logger:error_msg(Err_Msg, Err_Args),
    {reply, {error, unexpected_message}, StateName, StateData}.

-spec handle_info(term(), fsm_state_name(), term())
                -> {next_state, fsm_state_name(), state_data()}
                 | {stop, reason(), state_data()}.
handle_info({'DOWN', MonRef, process, Pid, _Info}, 
            _StateName, 
            #state_data{monitor_ref=MonRef, pid=Pid, name=Name} = _StateData) ->
    {next_state, 'DOWN', {Name, undefined}};
handle_info(Info, StateName, StateData) ->
    Err_Msg = "~p: unexpected info \"~p\", state data was ~p", 
    Err_Args = [?MODULE, Info, StateData],
    error_logger:error_msg(Err_Msg, Err_Args),
    {next_state, StateName, StateData}.

-spec terminate(term(), fsm_state_name(), state_data()) -> ok.
terminate(normal, _, _) -> ok;
terminate(shutdown, _, _) -> ok;
terminate(Reason, _StateName, StateData) ->
    Err_Msg = "~p:unexpected reason \"~p\", state data was ~p", 
    Err_Args = [?MODULE, Reason, StateData],
    error_logger:error_msg(Err_Msg, Err_Args),
    ok.

-spec code_change(term(), fsm_state_name(), state_data(), term()) -> 
    {ok, fsm_state_name(), state_data()}.
code_change(OldVsn, StateName, StateData, _Extra) ->
    Err_Msg = "~p:unexpected version \"~p\", state data was ~p",
    Err_Args = [?MODULE, OldVsn, StateData],
    error_logger:error_msg(Err_Msg, Err_Args), 
    {ok, StateName, StateData}.
