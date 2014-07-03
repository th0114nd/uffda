-module(uffda_service_fsm).
-behavior(gen_fsm).

%% External API
-export([start_link/2, fsm_name_from_service/1]).

-include("uffda.hrl").

%% FSM callbacks
-export([init/1, code_change/4, terminate/3,
         handle_event/3, handle_sync_event/4, handle_info/3]).

%% FSM Service States (only exported for FSM internal calling)

-define(STATE_STARTING_UP, 'STARTING_UP').
-define(STATE_DOWN,        'DOWN').
-define(STATE_UP,          'UP').

-type fsm_state_name() :: ?STATE_STARTING_UP | ?STATE_DOWN | ?STATE_UP.
-export([?STATE_STARTING_UP/2, ?STATE_DOWN/2, ?STATE_UP/2]).

%% FSM Internal state data
-record(state_data, {
          name        :: service_name(),
          pid         :: service_pid(),
          monitor_ref :: reference()
         }).

-type state_data()     :: #state_data{}.


%%----------------------------------------------------
%% External API
%%----------------------------------------------------

-spec fsm_name_from_service(service_name()) -> service_fsm().
fsm_name_from_service(Service_Name) ->
    list_to_atom(atom_to_list(Service_Name) ++ "_fsm").

-spec start_link(service_name(), service_pid()) -> {ok, service_fsm_pid()}.
start_link(Service_Name, Service_Pid) ->
    Service_Fsm = fsm_name_from_service(Service_Name),
    gen_fsm:start_link({local, Service_Fsm}, ?MODULE, {Service_Name, Service_Pid}, []).


%%----------------------------------------------------
%% States of a Service
%%----------------------------------------------------

%% A Service is starting up
-spec ?STATE_STARTING_UP(service_event(), state_data()) -> {next_state, fsm_state_name(), state_data()}.
?STATE_STARTING_UP(online, State_Data) -> {next_state, ?STATE_UP, State_Data};
?STATE_STARTING_UP(wait,   State_Data) -> {next_state, ?STATE_DOWN, State_Data};
?STATE_STARTING_UP(Event,  State_Data) ->
    log_unexpected_msg(?STATE_STARTING_UP, event, Event, State_Data),
    {next_state, ?STATE_STARTING_UP, State_Data}.

%% A Service is up and running
-spec ?STATE_UP(term(), state_data()) -> {next_state, fsm_state_name(), state_data()}.
?STATE_UP(offline, State_Data) -> {next_state, ?STATE_DOWN, State_Data};
?STATE_UP(Event,   State_Data) ->
    log_unexpected_msg(?STATE_UP, event, Event, State_Data),
    {next_state, ?STATE_UP, State_Data}.

%% A Service is down
-spec ?STATE_DOWN(term(), state_data()) -> {next_state, fsm_state_name(), state_data()}.
?STATE_DOWN(reset, State_Data) -> {next_state, ?STATE_STARTING_UP, State_Data};
?STATE_DOWN(Event, State_Data) ->
    log_unexpected_msg(?STATE_DOWN, event, Event, State_Data),
    {next_state, ?STATE_DOWN, State_Data}.

%%---------------------------------------------------
%% gen_fsm API callbacks 
%%---------------------------------------------------

-spec init({service_name(), service_pid()}) -> {ok, fsm_state_name(), state_data()}.
init({Service_Name, Service_Pid}) ->
    {ok, ?STATE_STARTING_UP, reinitialize(Service_Name, Service_Pid, #state_data{})}.

-spec reinitialize(service_name(), service_pid(), State1::state_data()) -> State2::state_data().
reinitialize(Service_Name, Service_Pid, #state_data{} = Old_State_Data) ->
    Mon_Ref = erlang:monitor(process, Service_Pid),
    Old_State_Data#state_data{name = Service_Name, pid = Service_Pid, monitor_ref = Mon_Ref}.

-spec handle_event(any(), State_Name, State_Data)
                  -> {next_state, State_Name, State_Data}
                         when State_Name :: fsm_state_name(),
                              State_Data :: state_data().
handle_event(Event, State_Name, State_Data) ->
    log_unexpected_msg(handle_event, event, Event, State_Data),
    {next_state, State_Name, State_Data}.

-type sync_event() :: {re_init, service_pid()} | get_current_status.
-spec handle_sync_event(sync_event(), {reference(), pid()}, State_Name, State_Data)
                       -> {next_state, State_Name, State_Data}
                              when State_Name :: fsm_state_name(),
                                   State_Data :: state_data().
handle_sync_event({re_init, Service_Pid}, _From, _State_Name, 
                  #state_data{name=Name} = State_Data) ->
    {reply, ok, ?STATE_STARTING_UP, reinitialize(Name, Service_Pid, State_Data)};
handle_sync_event(get_current_status, _From, State_Name, State_Data) ->
    {reply, status(State_Name), State_Name, State_Data};
handle_sync_event(Event, _From, State_Name, State_Data) ->
    log_unexpected_msg(handle_sync_event, event, Event, State_Data),
    {reply, {error, unexpected_message}, State_Name, State_Data}.

-type info_msg() :: {'DOWN', reference(), process, service_pid(), Reason::any()}.
-spec handle_info(info_msg(), fsm_state_name(), term())
                 -> {next_state, ?STATE_DOWN, state_data()}.
handle_info({'DOWN', Mon_Ref, process, Pid, _Info}, _State_Name, 
            #state_data{monitor_ref=Mon_Ref, pid=Pid} = State_Data) ->
    {next_state, ?STATE_DOWN, State_Data#state_data{monitor_ref=undefined, pid=undefined}};
handle_info(Info, State_Name, State_Data) ->
    log_unexpected_msg(handle_info, info, Info, State_Data),
    {next_state, State_Name, State_Data}.

-spec terminate(term(), fsm_state_name(), state_data()) -> ok.
terminate(normal,   _, _) -> ok;
terminate(shutdown, _, _) -> ok;
terminate(Reason, _State_Name, State_Data) ->
    log_unexpected_msg(terminate, reason, Reason, State_Data),
    ok.

-spec code_change(term(), fsm_state_name(), state_data(), term()) -> 
    {ok, fsm_state_name(), state_data()}.
code_change(Old_Vsn, State_Name, State_Data, _Extra) ->
    log_unexpected_msg(code_change, version, Old_Vsn, State_Data),
    {ok, State_Name, State_Data}.

%% Internal functions
log_unexpected_msg(Call, Type, Type_Value, State_Data) ->
    Err_Msg = "~p:~p unexpected ~p \"~p\", state data was ~p",
    Err_Args = [?MODULE, Call, Type, Type_Value, State_Data],
    error_logger:error_msg(Err_Msg, Err_Args).

status(?STATE_STARTING_UP) -> starting_up;
status(?STATE_DOWN)        -> down;
status(?STATE_UP)          -> up.
