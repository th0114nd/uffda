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
    "" = os:cmd(["echo ", Service_String, " ", Status_String, " | mail -s 'uffda ", Event_String, "' ", Address]),
    ok. 
