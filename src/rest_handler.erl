-module(rest_handler).

%% Cowboy REST callbacks
-export([init/3,
         allowed_methods/2]).

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>], Req, State}.

