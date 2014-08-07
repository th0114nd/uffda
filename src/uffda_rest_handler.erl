-module(uffda_rest_handler).

%% Cowboy REST callbacks
-export([init/3,
         rest_init/2,
         allowed_methods/2,
         no_body/2,
         output_plain_text/2,
         resource_exists/2,
         delete_resource/2,
         content_types_accepted/2,
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

%%------------------------------------------------------------------------------------
%% Cowboy REST callbacks
%%------------------------------------------------------------------------------------

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, []) ->
    {ok, Req, #state{name = get_name(Req), method = get_method(Req)}}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"DELETE">>, <<"PUT">>], Req, State}.

resource_exists(Req, State) ->
    {valid_name_method_conf(State#state.name, State#state.method), Req, State}. 

content_types_accepted(Req, State) ->
    {[
        {'*', no_body}
    ], Req, State}.

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

-spec which_services() -> binary(). 
which_services() ->
    list_to_binary([[atom_to_list(Service)|["\n"]] || Service <- uffda_client:which_services()]).

-spec service_status(atom()) -> binary().
service_status(Name) ->
    list_to_binary([atom_to_list(uffda_client:service_status(Name))|["\n"]]).

-spec delete_service(atom()) -> binary().
delete_service(Name) ->
    list_to_binary([atom_to_list(uffda_client:unregister_service(Name))|["\n"]]).

-spec add_service(atom()) -> boolean().
add_service(undefined) ->
    false;
add_service(Name) ->
    uffda_client:register_service(Name),
    service_exists(Name).

-spec service_exists(atom()) -> boolean().
service_exists(Name) ->
    lists:any(fun(Service) -> Name =:= Service end, uffda_client:which_services()). 

%%------------------------------------------------------------------------------------
%% IO callbacks
%%------------------------------------------------------------------------------------

no_body(Req, State) ->
    Response = dispatch_on_method(State#state.name, State#state.method),
    {Response, Req, State}.

output_plain_text(Req, State) ->
    Response = dispatch_on_method(State#state.name, State#state.method),
    {Response, Req, State}.

%%------------------------------------------------------------------------------------
%% Support functions
%%------------------------------------------------------------------------------------

get_name(Req) ->
    {Service_Bin, _} = cowboy_req:binding(name, Req),
    case Service_Bin of
        undefined -> undefined;
        _ -> binary_to_atom(Service_Bin, latin1)
    end.

get_method(Req) ->
    {Method_Bin, _} = cowboy_req:method(Req),
    binary_to_atom(Method_Bin, latin1).

-spec dispatch_on_method(atom(), atom()) -> binary() | boolean(). 
dispatch_on_method(undefined, 'GET') ->
    which_services();
dispatch_on_method(Name, 'GET') ->
    service_status(Name);
dispatch_on_method(Name, 'DELETE') ->
    delete_service(Name);
dispatch_on_method(Name, 'PUT') ->
    add_service(Name).

-spec valid_name_method_conf(atom(), atom()) -> boolean().
valid_name_method_conf(undefined, 'GET') -> true;
valid_name_method_conf(_, 'PUT') -> true;
valid_name_method_conf(Name, 'DELETE') ->
    service_exists(Name);
valid_name_method_conf(Name, 'POST') ->
    service_exists(Name);
valid_name_method_conf(Name, 'GET') ->
    service_exists(Name).
