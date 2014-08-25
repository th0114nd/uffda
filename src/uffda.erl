%% @doc
%%   The uffda application creates and manages a service registry
%%   which may be used to track the current running status of
%%   a set of  user-defined services. Each service is uniquely named
%%   and associated with a single pid that purports to be the primary
%%   active process which provides the service in question.
%%
%%   A VM node which runs services may be retro-fitted to send events
%%   to the uffda service registry, but the performance of any service
%%   should not be impacted if the service registry is down, crashes
%%   or is otherwise not available.
%%
%%   All interaction with the service registry must go through the
%%   uffda_client module. It represents the full External API of uffda.
%%   The functions in this module are only used when a node is started
%%   or stopped, or the service registry itself should be stopped.
%% @end
-module(uffda).
-behavior(application).

-export([start/0, stop/0]).
-export([start/2, start_phase/3, prep_stop/1, stop/1]).

%%-------------------------------------------------------------------
%% ADMIN API
%%-------------------------------------------------------------------
%% @doc
%%   Starts the application from the shell. It is assumed there is
%%   only one application registered as 'uffda'.
%% @end
-spec start() -> ok | {error, {already_started, ?MODULE}}.
start() -> application:start(?MODULE).

%% @doc
%%   Stops the 'uffda' application from the shell.
%% @end
-spec stop() -> ok.
stop() ->
    application:stop(?MODULE).


%%-------------------------------------------------------------------
%% BEHAVIOUR CALLBACKS
%%-------------------------------------------------------------------
%% @doc
%%   Starts the application in an OTP environment. Currently uffda
%%   is not expected to be run as a distributed erlang application.
%% @end
-spec start(any(), any()) -> {ok, pid()}.
start(_StartType, _StartArgs) -> 
    uffda_root_sup:start_link().

start_phase(listen, _, _) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {<<"/uffda-services/[:name]">>, uffda_rest_handler, []}
            ]}
        ]),
    {ok, _} = cowboy:start_http(http, 10, [{port, 8000}], [
        {env, [{dispatch, Dispatch}]}]),
    ct:log("Starting..."),
    ok.

prep_stop(_State) ->
    cowboy:stop_listener(http).

%% @doc
%%   Stops the application in an OTP environment.
%% @end
-spec stop(any()) -> no_return().
stop(_State) -> ok.
