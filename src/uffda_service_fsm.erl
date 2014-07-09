%% @doc
%%   The main modeling component of Uffda is the Service FSM which maintains
%%   the current running status of a Service. It uses a state machine and
%%   transitions as described in the following diagram:
%%
%%   <img src="images/states.png"/>
%% @end
-module(uffda_service_fsm).
-behavior(gen_fsm).

%% External API
-export([
         start_link/1,
         start_link/2,
         existing_fsm_name_from_service/1
        ]).

-include("uffda.hrl").

%% FSM callbacks
-export([init/1, code_change/4, terminate/3,
         handle_event/3, handle_sync_event/4, handle_info/3]).

%% FSM Service States (only exported for FSM internal calling)
-define(STATE_REGISTERED,      'STATE_REGISTERED').
-define(STATE_STARTING_UP,     'STATE_STARTING_UP').
-define(STATE_RESTARTING,      'STATE_RESTARTING').
-define(STATE_DELAYED_START,   'STATE_DELAYED_START').
-define(STATE_DELAYED_RESTART, 'STATE_DELAYED_RESTART').
-define(STATE_CRASHED,         'STATE_CRASHED').
-define(STATE_DOWN,            'STATE_DOWN').
-define(STATE_UP,              'STATE_UP').

-export([
         ?STATE_REGISTERED/2,
         ?STATE_STARTING_UP/2,
         ?STATE_RESTARTING/2,
         ?STATE_DELAYED_START/2,
         ?STATE_DELAYED_RESTART/2,
         ?STATE_CRASHED/2,
         ?STATE_DOWN/2,
         ?STATE_UP/2
        ]).

-type fsm_state_name() ::
        ?STATE_REGISTERED
      | ?STATE_STARTING_UP
      | ?STATE_RESTARTING
      | ?STATE_DELAYED_START
      | ?STATE_DELAYED_RESTART
      | ?STATE_CRASHED
      | ?STATE_DOWN
      | ?STATE_UP.

%% FSM Internal state data
-record(state_data, {
          name        :: service_name(),
          pid         :: service_pid(),
          monitor_ref :: reference()
         }).

-type state_data()     :: #state_data{}.

-define(MAX_START_UP_TIME, 30000).


%%----------------------------------------------------
%% External API
%%----------------------------------------------------

-spec existing_fsm_name_from_service(service_name())
                                    -> service_fsm() | {error, {not_registered, service_name()}}.
existing_fsm_name_from_service(Service_Name) ->
    Fsm_Name = fsm_name_from_service(Service_Name),
    try   list_to_existing_atom(Fsm_Name)
    catch error:badarg -> {error, {not_registered, Service_Name}}
    end.

-spec start_link(service_name()) -> {ok, service_fsm_pid()}.
start_link(Service_Name) ->
    Service_Fsm = list_to_atom(fsm_name_from_service(Service_Name)),
    gen_fsm:start_link({local, Service_Fsm}, ?MODULE, {Service_Name}, []).

-spec start_link(service_name(), service_pid()) -> {ok, service_fsm_pid()}.
start_link(Service_Name, Service_Pid) ->
    Service_Fsm = list_to_atom(fsm_name_from_service(Service_Name)),
    gen_fsm:start_link({local, Service_Fsm}, ?MODULE, {Service_Name, Service_Pid}, []).

fsm_name_from_service(Service_Name) ->
    atom_to_list(Service_Name) ++ "_uffda_service_fsm".


%%----------------------------------------------------
%% States of a Service
%%----------------------------------------------------

%% Service is registered for the first time
-spec ?STATE_REGISTERED(service_event(), state_data())
                       -> {next_state, fsm_state_name(), state_data()}.

?STATE_REGISTERED({starting, Service_Pid}, #state_data{name=Service_Name} = State_Data) ->
    set_starting_up_with_timeout(reinitialize(Service_Name, Service_Pid, State_Data));

?STATE_REGISTERED(online,   State_Data) -> {next_state, ?STATE_UP,   State_Data};
?STATE_REGISTERED(offline,  State_Data) -> {next_state, ?STATE_DOWN, State_Data};
?STATE_REGISTERED(Event,    State_Data) ->
    log_unexpected_msg(?STATE_REGISTERED, event, Event, State_Data),
    {next_state, ?STATE_REGISTERED, State_Data}.

%% Service is starting up
-spec ?STATE_STARTING_UP (service_event(), state_data())
                         -> {next_state, fsm_state_name(), state_data()}.

?STATE_STARTING_UP (Event, State_Data) -> starting_transition(Event, State_Data, ?STATE_STARTING_UP).

-spec ?STATE_RESTARTING  (service_event(), state_data())
                         -> {next_state, fsm_state_name(), state_data()}.

?STATE_RESTARTING  (Event, State_Data) -> starting_transition(Event, State_Data, ?STATE_RESTARTING).

%% Multiple starting events is like a ping that provides more time to startup.
starting_transition({starting, Service_Pid}, #state_data{name=Service_Name} = State_Data, ?STATE_STARTING_UP) ->
    set_starting_up_with_timeout (reinitialize(Service_Name, Service_Pid, State_Data));
starting_transition({starting, Service_Pid}, #state_data{name=Service_Name} = State_Data, ?STATE_RESTARTING)  ->
    set_restarting_with_timeout  (reinitialize(Service_Name, Service_Pid, State_Data));

starting_transition(timeout,  State_Data, ?STATE_STARTING_UP) -> {next_state, ?STATE_DELAYED_START,   State_Data};
starting_transition(timeout,  State_Data, ?STATE_RESTARTING)  -> {next_state, ?STATE_DELAYED_RESTART, State_Data};
starting_transition(online,   State_Data, _Start_Or_Restart)  -> {next_state, ?STATE_UP,              State_Data};
starting_transition(offline,  State_Data, _Start_Or_Restart)  -> {next_state, ?STATE_DOWN,            State_Data};

%% Unexpected events also breathe new life into the starting timeout.
starting_transition(Event,    State_Data, ?STATE_STARTING_UP) ->
    log_unexpected_msg(?STATE_STARTING_UP, event, Event, State_Data),
    set_starting_up_with_timeout(State_Data, ?MAX_START_UP_TIME);
starting_transition(Event,    State_Data, ?STATE_RESTARTING) ->
    log_unexpected_msg(?STATE_RESTARTING,  event, Event, State_Data),
    set_restarting_with_timeout(State_Data,  ?MAX_START_UP_TIME).

set_starting_up_with_timeout(State_Data) ->
    set_starting_up_with_timeout(State_Data, ?MAX_START_UP_TIME).

set_starting_up_with_timeout(State_Data, Timeout) ->
    {next_state, ?STATE_STARTING_UP, State_Data, Timeout}.

set_restarting_with_timeout(State_Data) ->
    set_restarting_with_timeout(State_Data, ?MAX_START_UP_TIME).

set_restarting_with_timeout(State_Data, Timeout) ->
    {next_state, ?STATE_RESTARTING, State_Data, Timeout}.

%% Service is is taking too long to start up
-spec ?STATE_DELAYED_START   (service_event(), state_data())
                             -> {next_state, fsm_state_name(), state_data()}.

?STATE_DELAYED_START   (Event, State_Data) -> delayed_transition(Event, State_Data, ?STATE_DELAYED_START).

-spec ?STATE_DELAYED_RESTART (service_event(), state_data())
                             -> {next_state, fsm_state_name(), state_data()}.

?STATE_DELAYED_RESTART (Event, State_Data) -> delayed_transition(Event, State_Data, ?STATE_DELAYED_RESTART).

delayed_transition({starting, Service_Pid}, #state_data{name=Service_Name} = State_Data, ?STATE_DELAYED_START) ->
    set_starting_up_with_timeout(reinitialize(Service_Name, Service_Pid, State_Data));
delayed_transition({starting, Service_Pid}, #state_data{name=Service_Name} = State_Data, ?STATE_DELAYED_RESTART) ->
    set_restarting_with_timeout(reinitialize(Service_Name, Service_Pid, State_Data));

delayed_transition(online,   State_Data, _Start_Or_Restart)      -> {next_state, ?STATE_UP,   State_Data};
delayed_transition(offline,  State_Data, _Start_Or_Restart)      -> {next_state, ?STATE_DOWN, State_Data};
delayed_transition(Event,    State_Data,  Start_Or_Restart)      ->
    log_unexpected_msg(Start_Or_Restart, event, Event, State_Data),
    {next_state, Start_Or_Restart, State_Data}.

%% Service can toggle between restarting/up/down/crashed.
-spec ?STATE_UP      (term(), state_data()) -> {next_state, fsm_state_name(), state_data()}.
?STATE_UP      (Event, State_Data) -> up_down_transition(Event, State_Data, ?STATE_UP).

-spec ?STATE_DOWN    (term(), state_data()) -> {next_state, fsm_state_name(), state_data()}.
?STATE_DOWN    (Event, State_Data) -> up_down_transition(Event, State_Data, ?STATE_DOWN).

-spec ?STATE_CRASHED (term(), state_data()) -> {next_state, fsm_state_name(), state_data()}.
?STATE_CRASHED (Event, State_Data) -> up_down_transition(Event, State_Data, ?STATE_CRASHED).

up_down_transition({starting, Service_Pid}, #state_data{name=Service_Name} = State_Data, _Current_State) ->
    set_restarting_with_timeout(reinitialize(Service_Name, Service_Pid, State_Data));

up_down_transition(online,   State_Data, _Current_State) -> {next_state, ?STATE_UP,   State_Data};
up_down_transition(offline,  State_Data, _Current_State) -> {next_state, ?STATE_DOWN, State_Data};
up_down_transition(Event,    State_Data,  Current_State) ->
    log_unexpected_msg(Current_State, event, Event, State_Data),
    {next_state, Current_State, State_Data}.


%%---------------------------------------------------
%% gen_fsm API callbacks 
%%---------------------------------------------------

-spec init({service_name()} | {service_name(), service_pid()}) -> {ok, fsm_state_name(), state_data()}.
init({Service_Name}) ->
    {ok, ?STATE_REGISTERED, #state_data{name=Service_Name}};
init({Service_Name, Service_Pid}) ->
    {ok, ?STATE_REGISTERED, reinitialize(Service_Name, Service_Pid, #state_data{})}.

-spec reinitialize(service_name(), service_pid(), State1::state_data()) -> State2::state_data().
reinitialize(Service_Name, Service_Pid, #state_data{monitor_ref = Old_Mon_Ref} = Old_State_Data) ->
    true = Old_Mon_Ref =:= undefined
        orelse erlang:demonitor(Old_Mon_Ref, [flush]),
    Mon_Ref = erlang:monitor(process, Service_Pid),
    Old_State_Data#state_data{name = Service_Name, pid = Service_Pid, monitor_ref = Mon_Ref}.

-spec handle_event(any(), State_Name, State_Data)
                  -> {next_state, State_Name, State_Data}
                         when State_Name :: fsm_state_name(),
                              State_Data :: state_data().
handle_event(Event, State_Name, State_Data) ->
    log_unexpected_msg(handle_event, event, Event, State_Data),
    {next_state, State_Name, State_Data}.

-type sync_event() :: {re_init, service_pid()} | current_status.
-spec handle_sync_event(sync_event(), {pid(), Tag :: term()}, State_Name, State_Data)
                       -> {reply, ok | service_status(), State_Name, State_Data}
                              when State_Name :: fsm_state_name(),
                                   State_Data :: state_data().

%% Re-init with the same process that is already being monitored...
handle_sync_event({re_init, Service_Pid}, _From, State_Name, #state_data{pid=Service_Pid} = State_Data)
  when State_Name =/= ?STATE_REGISTERED ->
    {reply, ok, ?STATE_RESTARTING, State_Data, ?MAX_START_UP_TIME};

%% Re-init with a new process to monitor...
handle_sync_event({re_init, Service_Pid}, _From, State_Name, #state_data{name=Name} = State_Data)
  when State_Name =/= ?STATE_REGISTERED ->
    New_State_Data = reinitialize(Name, Service_Pid, State_Data),
    {reply, ok, ?STATE_RESTARTING, New_State_Data, ?MAX_START_UP_TIME};

%% Request to get the current status...
handle_sync_event(current_status, _From, State_Name, State_Data) ->
    {reply, status(State_Name), State_Name, State_Data};

%% Request for a service name...
handle_sync_event(get_service_name, _From, State_Name, #state_data{name=Name} = State_Data) ->
    {reply, Name, State_Name, State_Data};

%% Some unknown request.
handle_sync_event(Event, _From, State_Name, State_Data) ->
    log_unexpected_msg(handle_sync_event, event, Event, State_Data),
    {reply, {error, unexpected_message}, State_Name, State_Data}.

-type info_msg() :: {'DOWN', reference(), process, service_pid(), Reason::any()}.
-spec handle_info(info_msg(), fsm_state_name(), term())
                 -> {next_state, ?STATE_DOWN, state_data()}.

%% Service went down normally...
handle_info({'DOWN', Mon_Ref, process, Pid, normal}, _State_Name, 
            #state_data{monitor_ref=Mon_Ref, pid=Pid} = State_Data) ->
    {next_state, ?STATE_DOWN,    State_Data#state_data{monitor_ref=undefined, pid=undefined}};

%% Service went down with a reason other than 'normal'...
handle_info({'DOWN', Mon_Ref, process, Pid, _Reason}, _State_Name, 
            #state_data{monitor_ref=Mon_Ref, pid=Pid} = State_Data) ->
    {next_state, ?STATE_CRASHED, State_Data#state_data{monitor_ref=undefined, pid=undefined}};

%% Some unknown request.
handle_info(Info, State_Name, State_Data) ->
    log_unexpected_msg(handle_info, info, Info, State_Data),
    {next_state, State_Name, State_Data}.

-spec terminate(term(), fsm_state_name(), state_data()) -> ok.
terminate(normal,   _State_Name, _State_Data) -> ok;
terminate(shutdown, _State_Name, _State_Data) -> ok;
terminate(Reason,   _State_Name,  State_Data) ->
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

-spec status(fsm_state_name()) -> service_status().
status(?STATE_REGISTERED)      -> registered;
status(?STATE_STARTING_UP)     -> starting_up;
status(?STATE_RESTARTING)      -> restarting;
status(?STATE_DELAYED_START)   -> slow_start;
status(?STATE_DELAYED_RESTART) -> slow_restart;
status(?STATE_CRASHED)         -> crashed;
status(?STATE_DOWN)            -> down;
status(?STATE_UP)              -> up.
