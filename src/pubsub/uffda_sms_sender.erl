-module(uffda_sms_sender).

-include("uffda.hrl").
-author('tholland@tigertext.com').
-behavior(uffda_publisher).

-export([send/2]).
-spec send(event(), ass()) -> ok.
send(Event, #ass{address = Address,
                 service = Service,
                 status = Status})
  when is_atom(Event), is_list(Address), is_atom(Service), is_atom(Status) ->
    10 = length(Address),
    Msg = ["uffda/", atom_to_list(Event), ": ", atom_to_list(Service), " is now ", atom_to_list(Status)],
    Cmd = ["curl -s http://textbelt.com/text -d number=", Address, " -d \"message=", Msg, "\" | less"],
    {Json} = jiffy:decode(os:cmd(Cmd)),
    case proplists:get_value(<<"success">>, Json) of
        true -> ok;
        false -> {error, proplists:get_value(<<"message">>, Json)}
                 
    end.
