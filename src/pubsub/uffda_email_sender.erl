-module(uffda_email_sender).

-include("uffda.hrl").
-author('tholland@tigertext.com').
-behavior(uffda_publisher).

-export([send/2]).
-spec send(event(), ass()) -> ok.
send(Event, #ass{address = Address,
                 service = Service,
                 status = Status})
  when is_atom(Event), is_list(Address), is_atom(Service), is_atom(Status) ->
    [Event_String, Service_String, Status_String] = [atom_to_list(Item) || Item <- [Event, Service, Status]],
    "" = os:cmd(io_lib:format("echo ~s ~s | mail -s 'uffda/~s' ~s", [Service_String, Status_String, Event_String, Address]),
    ok. 
