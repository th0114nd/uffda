-module(uffda).
-behavior(application).

-export([start/0, stop/0]).
-export([start/2, stop/1]).

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
stop() -> application:stop(?MODULE).


%%-------------------------------------------------------------------
%% BEHAVIOUR CALLBACKS
%%-------------------------------------------------------------------
%% @doc
%%   Starts the application in an OTP environment. Currently uffda
%%   is not expected to be run as a distributed erlang application.
%% @end
-spec start(any(), any()) -> {ok, pid()}.
start(_StartType, _StartArgs) -> uffda_registry_sup:start_link().

%% @doc
%%   Stops the application in an OTP environment.
%% @end
-spec stop(any()) -> no_return().
stop(_State) -> ok.
