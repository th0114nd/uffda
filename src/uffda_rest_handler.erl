-module(uffda_rest_handler).

%% Cowboy REST callbacks
-export([init/3,
         rest_init/2,
         allowed_methods/2,
         resource_exists/2,
         content_types_provided/2,
         terminate/3]).

%% Calls to uffda
-export([which_services/2,
         service_status/2,
         hello_to_text/2
        ]).

-record(state,
        {fn :: atom()}).

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, [Function]) ->
    {ok, Req, #state{fn = Function}}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

resource_exists(Req, State) ->
    {Service_Bin, Req2} = cowboy_req:binding(name, Req),
    Status = 
        case Service_Bin of 
            undefined -> undefined;
            _ -> uffda_client:service_status(binary_to_atom(Service_Bin, latin1))
        end,
    case Status of
        {error, _} -> {false, Req2, State};
        _ -> {true, Req2, State}
    end.

content_types_provided(Req, State) ->
    {[
        {<<"text/plain">>, State#state.fn}
    ], Req, State}.

which_services(Req, State) ->
    IO_Services_List = [atom_to_list(Service) ++ "\n" || Service <- uffda_client:which_services()],
    {list_to_binary(IO_Services_List), Req, State}.

service_status(Req, State) ->
    {Service_Bin, Req2} = cowboy_req:binding(name, Req),
    Service_Name = binary_to_atom(Service_Bin, latin1),
    {list_to_binary(atom_to_list(uffda_client:service_status(Service_Name)) ++ "\n"), Req2, State}.

hello_to_text(Req, State) ->
    {<<"Hello World!\n">>, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.
