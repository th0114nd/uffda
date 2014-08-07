-module(uffda_rest_handler).

%% Cowboy REST callbacks
-export([init/3,
         rest_init/2,
         allowed_methods/2,
         output_plain_text/2,
         resource_exists/2,
         delete_resource/2,
         content_types_provided/2,
         terminate/3]).

%% Calls to uffda
-export([which_services/0,
         service_status/1,
         delete_service/1
        ]).

-record(state,
        {name :: atom(),
        method :: binary()}).

%%---------------------------------------------------------------------------------------
%% Cowboy REST callbacks
%%---------------------------------------------------------------------------------------

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, []) ->
    {Service_Bin, _} = cowboy_req:binding(name, Req),
    Name = case Service_Bin of
               undefined -> undefined;
               _ -> binary_to_atom(Service_Bin, latin1)
           end,
    {Method_Bin, _} = cowboy_req:method(Req),
    Method = binary_to_atom(Method_Bin, latin1),
    {ok, Req, #state{name = Name, method = Method}}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"DELETE">>], Req, State}.

resource_exists(Req, State) ->
    {valid_name_method_conf(State#state.name, State#state.method), Req, State}. 

content_types_provided(Req, State) ->
    {[
        {<<"text/plain">>, output_plain_text}
    ], Req, State}.

delete_resource(Req, State) ->
    dispatch_on_method(State#state.name, State#state.method),
    {not service_exists(State#state.name), Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.
%%------------------------------------------------------------------------------------
%% Communication with uffda
%%------------------------------------------------------------------------------------

which_services() ->
    [[atom_to_list(Service)|["\n"]] || Service <- uffda_client:which_services()].

service_status(Name) ->
    [atom_to_list(uffda_client:service_status(Name))|["\n"]].

delete_service(Name) ->
    [atom_to_list(uffda_client:unregister_service(Name))|["\n"]].

service_exists(Name) ->
    lists:any(fun(Service) -> Name =:= Service end, uffda_client:which_services()). 

%%------------------------------------------------------------------------------------
%% IO formats
%%------------------------------------------------------------------------------------

output_plain_text(Req, State) ->
    Response = dispatch_on_method(State#state.name, State#state.method),
    {list_to_binary(Response), Req, State}.

%%------------------------------------------------------------------------------------
%% Support functions
%%------------------------------------------------------------------------------------

dispatch_on_method(undefined, 'GET') ->
    which_services();
dispatch_on_method(Name, 'GET') ->
    service_status(Name);
dispatch_on_method(Name, 'DELETE') ->
    delete_service(Name).

valid_name_method_conf(undefined, 'GET') -> true;
valid_name_method_conf(_, 'PUT') -> true;
valid_name_method_conf(Name, 'DELETE') ->
    service_exists(Name);
valid_name_method_conf(Name, 'POST') ->
    service_exists(Name);
valid_name_method_conf(Name, 'GET') ->
    service_exists(Name).
