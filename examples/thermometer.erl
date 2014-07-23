%% @doc
%%   An example service that is not supervised. It calls register,
%%   starting_service, and set_service_online the way they are
%%   intended to be used.
%% @end
-module(thermometer).
-behaviour(gen_server).

%% External API
-export([start_link/0, get_temp/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, therm).


%%----------------------
%% External API
%%----------------------

-spec start_link() -> {ok, pid()}.
%% @doc
%%   Start a gen_server process for the example temperature
%%   service. Register the service name first, then start the
%%   gen_server. When the gen_server pid is successfully
%%   returned, set the service online.
%%
%%   If this service were supervised, any restarts would call
%%   the start_link/0 function again. This would result in
%%   multiple calls to register. No harm would be caused by
%%   by that approach, but it would be better to try to
%%   call register inside the supervisor initialization
%%   instead to avoid spurious calls.
%%
%%   This service is a simulation of a thermometer reading
%%   application. It carries an internal temperature state
%%   and periodically changes the temperature by a random
%%   amount.
%% @end
start_link() ->
    ok = uffda_client:register_service(?SERVER),
    {ok, Pid} = gen_server:start({local, ?SERVER}, ?MODULE, {}, []),
    ok = uffda_client:set_service_online(?SERVER),
    {ok, Pid}.

%% @doc
%%   Return the current temperature as reported by the service.
%%   This is the full external API for this application.
%% @end
get_temp() ->
    gen_server:call(?SERVER, get_temp).

%% @doc
%%   Shutdown the thermometer gracefully. This approach allows
%%   us to test that uffda reports the service as 'down' rather
%%   than 'crashed'.
%% @end
stop() ->
    gen_server:call(?SERVER, stop).


%%----------------------
%% Internal callbacks
%%----------------------

-define(CHANGE_TEMP_TIME, 3000).

-spec init({}) -> integer().
%% @hidden
%% @doc
%%   Seed the random generator for this process and return
%%   a starting temperature as the initial internal state.
%% @end
init({}) ->
    _ = uffda_client:starting_service(?SERVER, self()),
    _ = random:seed(),
    {ok, random:uniform(60), ?CHANGE_TEMP_TIME}.

%% @hidden
handle_call(get_temp, _From, Current_Temp) ->
    {reply, Current_Temp, Current_Temp, ?CHANGE_TEMP_TIME};
handle_call(stop, _From, Current_Temp) ->
    {stop, normal, stopped, Current_Temp}.

%% @hidden
handle_info(timeout, Current_Temp) ->
    {noreply, Current_Temp + random:uniform(6) - 3, ?CHANGE_TEMP_TIME}.

%% @hidden
handle_cast(_Msg, Current_Temp) ->
    {noreply, Current_Temp, ?CHANGE_TEMP_TIME}.

%% @hidden
terminate(_Reason, _State) -> ok.

%% @hidden
code_change(_OldVsn, State, _Extra) -> {ok, State}.
