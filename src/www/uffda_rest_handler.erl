-module(uffda_rest_handler).
-author('pierce.mt92@gmail.com').

%% Cowboy REST callbacks
-export([init/3,
         rest_init/2,
         allowed_methods/2,
         accept_app/2,
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

-include("uffda.hrl").

-type req() :: cowboy_req:req().

%%------------------------------------------------------------------------------------
%% Cowboy REST callbacks
%%------------------------------------------------------------------------------------

-spec init(any(), req(), [any()]) -> {upgrade, protocol, cowboy_rest}.
init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

-spec rest_init(req(), []) -> {ok, req(), #state{}}.
rest_init(Req, []) ->
    {Method, Req2} = cowboy_req:method(Req),
    {ok, Req2, #state{name = get_name(Req), method = Method}}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"DELETE">>, <<"PUT">>, <<"POST">>], Req, State}.

-spec resource_exists(req(), #state{}) -> {boolean(), req(), #state{}}.
resource_exists(Req, #state{name = Name, method = Method} = State) ->
    {valid_name_method_conf(Name, Method), Req, State}.

-spec content_types_provided(req(), #state{}) -> {[{atom() | binary(), atom()}], req(), #state{}}.
content_types_accepted(Req, #state{} = State) ->
    {[
        {<<"application/x-www-form-urlencoded">>, accept_app},
        {'*', no_body}
    ], Req, State}.

-spec content_types_accepted(req(), #state{}) -> {[{binary(), atom()}], req(), #state{}}.
content_types_provided(Req, State) ->
    {[
        {<<"text/plain">>, output_plain_text}
    ], Req, State}.

-spec delete_resource(req(), #state{}) -> {boolean(), req(), #state{}}.
delete_resource(Req, #state{name = Name, method = Method} = State) ->
    _ = dispatch_on_method(Name, Method),
    {not service_exists(Name), Req, State}.

-spec terminate(any(), req(), #state{}) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.
%%------------------------------------------------------------------------------------
%% Communication with uffda
%%------------------------------------------------------------------------------------

-spec which_services() -> binary(). 
which_services() ->
    list_to_binary([atom_to_list(Service) ++ "\n"
                   || Service <- uffda_client:which_services()]).

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
    case uffda_client:register_service(Name) of
        ok -> service_exists(Name);
        {error, already_started} -> false;
        {error, {not_started, _}} -> exit(internal_server_error)
    end.

-spec update_service_status(atom(), binary()) -> boolean().
update_service_status(Name, <<"start">>) ->
    safe_update(Name, starting_service, 'starting_up');
update_service_status(Name, <<"set_online">>) ->
    safe_update(Name, set_service_online, 'up');
update_service_status(Name, <<"set_offline">>) ->
    safe_update(Name, set_service_offline, 'down').

-spec safe_update(atom(), service_name(), service_status()) -> boolean().
safe_update(Name, Fun, Expected) ->
    case uffda_client:Fun(Name) of
        ok -> uffda_client:service_status(Name) =:= Expected;
        {error, {not_registered, Name}} -> false;
        {error, {not_started, uffda_service_registry}} ->
            error_logger:error_msg("Uffda registry not started."),
            exit(internal_server_error)
    end.
-spec service_exists(atom()) -> boolean().
service_exists(Name) ->
    lists:any(fun(Service) -> Name =:= Service end, uffda_client:which_services()). 

%%------------------------------------------------------------------------------------
%% IO callbacks
%%------------------------------------------------------------------------------------

-spec no_body(req(), #state{}) -> {boolean() | binary(), req(), #state{}}.
no_body(Req, #state{name = Name, method = Method} = State) ->
    Response = dispatch_on_method(Name, Method),
    {Response, Req, State}.

-spec accept_app(req(), #state{}) -> {boolean(), req(), #state{}}.
accept_app(Req, #state{name = Name, method = Method} = State) ->
    {ok, Body, Req2} = cowboy_req:body_qs(Req),
    Result = validate_body(Body) andalso
             begin
                Update = proplists:get_value(<<"update">>, Body),
                dispatch_on_method(Name, Method, Update)
             end,
    {Result, Req2, State}.

-spec output_plain_text(req(), #state{}) -> {boolean() | binary(), req(), #state{}}.
output_plain_text(Req, #state{name = Name, method = Method} = State) ->
    {dispatch_on_method(Name, Method), Req, State}.

%%------------------------------------------------------------------------------------
%% Support functions
%%------------------------------------------------------------------------------------

-spec get_name(req()) -> atom().
get_name(Req) ->
    {Service_Bin, _} = cowboy_req:binding(name, Req),
    case Service_Bin of
        undefined -> undefined;
        _ -> binary_to_atom(Service_Bin, latin1)
    end.

-spec dispatch_on_method(atom(), binary()) -> binary() | boolean().
-spec dispatch_on_method(atom(), binary(), binary()) -> boolean().
dispatch_on_method(undefined, <<"GET">>) -> which_services();
dispatch_on_method(Name, <<"GET">>) -> service_status(Name);
dispatch_on_method(Name, <<"DELETE">>) -> delete_service(Name);
dispatch_on_method(Name, <<"PUT">>) -> add_service(Name).
dispatch_on_method(Name, <<"POST">>, Update) ->
    update_service_status(Name, Update). %proplists:get_value(<<"update">>, Body)).

-spec valid_name_method_conf(atom(), <<_:24, _:_*8>>) -> boolean().
valid_name_method_conf(undefined, <<"GET">>) -> true;
valid_name_method_conf(_, <<"PUT">>) -> true;
valid_name_method_conf(Name, <<"DELETE">>) -> service_exists(Name);
valid_name_method_conf(Name, <<"POST">>) -> service_exists(Name);
valid_name_method_conf(Name, <<"GET">>) -> service_exists(Name).

-spec validate_body(proplists:proplist()) -> boolean().
validate_body(Body) ->
    Command = proplists:get_value(<<"update">>, Body),
    Valid_Commands = [<<"start">>, <<"set_online">>, <<"set_offline">>],
    lists:member(Command, Valid_Commands).
