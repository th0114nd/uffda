-module(rest_handler).

%% Cowboy REST callbacks
-export([init/3,
         allowed_methods/2,
         handle/2,
         terminate/3]).

init(_Transport, Req, _Opts) ->
    {ok, Req, undefined}.
%    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>], Req, State}.

handle(Req, State) ->
    {ok, Req2} = cowboy_req:reply(200, [
        {<<"content-type">>, <<"text/plain">>}
    ], <<"rest hello world">>, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
