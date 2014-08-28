-module(uffda_pid_sender).

-include("uffda.hrl").
-author('tholland@tigertext.com').
-behavior(uffda_publisher).

-export([send/2]).
-spec send(event(), ass()) -> ok.
send(Event, #ass{address = Address,
                 service = Service,
                 status = Status})
  when is_atom(Event), is_pid(Address), is_atom(Service), is_atom(Status) ->
    Address ! {Event, {Service, Status}},
    ok. 
