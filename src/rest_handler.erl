-module(rest_handler).

%% Cowboy REST callbacks
-export([init/3,
         allowed_methods/2,
         content_types_provided/2,
         handle/2,
         terminate/3]).

-export([hello_to_text/2]).

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"text/plain">>, hello_to_text}
    ], Req, State}.

handle(Req, State) ->
    {ok, Req2} = cowboy_req:reply(200, [
        {<<"content-type">>, <<"text/plain">>}
    ], <<"rest hello world">>, Req),
    {ok, Req2, State}.

hello_to_text(Req, State) ->
    {<<"Rest hello world">>, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.
