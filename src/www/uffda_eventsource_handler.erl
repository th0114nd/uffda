-module(uffda_eventsource_handler).

-export([init/3]).
-export([info/3]).
-export([terminate/3]).

init(_Transport, Req, []) ->
    {{Name, Status}, Req2} = cowboy_req:binding(name, Req),
    Headers = [{<<"content-type">>, <<"text/event-stream">>}],
    {ok, Req3} = cowboy_req:chunked_reply(200, Headers, Req2),
    ok = cowboy_req:chunk(["event: subscription-start\n",
                      "id: ", atom_to_list(Name), "\n",
                      "data: ", atom_to_list(Status), "\n\n"], Req3),
    uffda_subscription:subscribe(sse, self(), Name, Status), 
    {loop, Req3, Name}.

info({update, {Name, Status}}, Req, Name) ->
    ok = cowboy_req:chunk(["event: update\n",
                           "id: ", atom_to_list(Name), "\n",
                           "data: ", atom_to_list(Status), "\n\n"], Req),
    {loop, Req, Name}.

terminate(_Reason, _Req, _State) ->
    ok.
