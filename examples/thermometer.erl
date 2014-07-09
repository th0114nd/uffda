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
start_link() ->
    ok = uffda_client:register_service(?SERVER),
    {ok, Pid} = gen_server:start({local, ?SERVER}, ?MODULE, {}, []),
    ok = uffda_client:set_service_online(?SERVER),
    {ok, Pid}.

%% A method of querying the temperature service.
get_temp() ->
    gen_server:call(?SERVER, get_temp).

stop() ->
    gen_server:call(?SERVER, stop).


%%----------------------
%% Internal callbacks
%%----------------------

-define(CHANGE_TEMP_TIME, 3000).

-spec init({}) -> integer().
init({}) ->
    _ = uffda_client:starting_service(?SERVER, self()),
    _ = random:seed(),
    {ok, random:uniform(60), ?CHANGE_TEMP_TIME}.

handle_call(get_temp, _From, Current_Temp) ->
    {reply, Current_Temp, Current_Temp, ?CHANGE_TEMP_TIME};
handle_call(stop, _From, Current_Temp) ->
    {stop, normal, stopped, Current_Temp}.

handle_info(timeout, Current_Temp) ->
    {noreply, Current_Temp + random:uniform(6) - 3, ?CHANGE_TEMP_TIME}.

handle_cast(_Msg, Current_Temp) ->
    {noreply, Current_Temp, ?CHANGE_TEMP_TIME}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
