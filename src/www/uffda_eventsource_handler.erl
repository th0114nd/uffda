-module(uffda_eventsource_handler).

-export([init/3]).
-export([info/3]).
-export([terminate/3]).

init(_Transport, Req, []) ->
    {{Name, Status}, Req2} = cowboy_req:binding(name, Req),
    Headers = [{<<"content-type">>, <<"text/event-stream">>}],
    {ok, Req3} = cowboy_req:chunked_reply(200, Headers, Req2),
    uffda_subscription:subscribe(sse, self(), Name, Status), 
    {loop, Req3, Name}.

info({Event, {Name, Status}}, Req, Name) ->
    ok = cowboy_req:chunk(["event: ", atom_to_list(Event), "\n",
                           "id: ", atom_to_list(Name), "\n",
                           "data: ", atom_to_list(Status), "\n\n"], Req),
    case Event of
        Ev when Ev /= unsubscribing -> {loop, Req, Name};
        unsubscribing -> {ok, Req, Name}
    end.

terminate(_Reason, _Req, _State) ->
    ok.
