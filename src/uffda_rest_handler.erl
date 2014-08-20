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

%%------------------------------------------------------------------------------------
%% Cowboy REST callbacks
%%------------------------------------------------------------------------------------

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, []) ->
    {Method, Req2} = cowboy_req:method(Req),
    {ok, Req2, #state{name = get_name(Req), method = Method}}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"DELETE">>, <<"PUT">>, <<"POST">>], Req, State}.

resource_exists(Req, State) ->
    {valid_name_method_conf(State#state.name, State#state.method), Req, State}. 

content_types_accepted(Req, State) ->
    {[
        {<<"application/x-www-form-urlencoded">>, accept_app},
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
    list_to_binary([[atom_to_list(Service)|["\n"]]
                   || Service <- uffda_client:which_services()]).

-spec service_status(atom()) -> binary().
service_status(Name) ->
    list_to_binary([atom_to_list(uffda_client:service_status(Name))|["\n"]]).

-spec delete_service(atom()) -> binary().
delete_service(Name) ->
    list_to_binary([atom_to_list(uffda_client:unregister_service(Name))|["\n"]]).

-spec add_service(atom(), binary()) -> boolean().
add_service(undefined, _) ->
    false;
add_service(Name, undefined) ->
    uffda_client:register_service(Name),
    service_exists(Name);
add_service(Name, Timeout) ->
    uffda_client:register_service(Name, [binary_to_integer(Timeout)]),
    service_exists(Name).

update_service_status(Name, <<"start">>, Body) ->
    Pid = list_to_pid(binary_to_list(proplists:get_value(<<"pid">>, Body))),
    uffda_client:starting_service(Name, Pid),
    uffda_client:service_status(Name) =:= 'starting_up' orelse
    uffda_client:service_status(Name) =:= 'restarting';
update_service_status(Name, <<"set_online">>, _) ->
    uffda_client:set_service_online(Name),
    uffda_client:service_status(Name) =:= 'up';
update_service_status(Name, <<"set_offline">>, _) ->
    uffda_client:set_service_offline(Name),
    uffda_client:service_status(Name) =:= 'down'.

-spec service_exists(atom()) -> boolean().
service_exists(Name) ->
    lists:any(fun(Service) -> Name =:= Service end, uffda_client:which_services()). 

%%------------------------------------------------------------------------------------
%% IO callbacks
%%------------------------------------------------------------------------------------

no_body(Req, State) ->
    Response = dispatch_on_method(State#state.name, State#state.method),
    {Response, Req, State}.

accept_app(Req, State) ->
    {ok, Body, Req2} = cowboy_req:body_qs(Req),
    Result = validate(State#state.method, Body) andalso
             dispatch_on_method_with_body(State#state.name,
                                          State#state.method,
                                          Body),
    {Result, Req2, State}.

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

-spec dispatch_on_method(atom(), atom()) -> binary() | boolean(). 
dispatch_on_method(undefined, <<"GET">>) ->
    which_services();
dispatch_on_method(Name, <<"GET">>) ->
    service_status(Name);
dispatch_on_method(Name, <<"DELETE">>) ->
    delete_service(Name).

-spec dispatch_on_method_with_body(atom(), binary(), proplists:proplist()) -> boolean().
dispatch_on_method_with_body(Name, <<"POST">>, Body) ->
    update_service_status(Name, proplists:get_value(<<"update">>, Body), Body);
dispatch_on_method_with_body(Name, <<"PUT">>, Body) ->
    add_service(Name, proplists:get_value(<<"timeout">>, Body, undefined)).

-spec valid_name_method_conf(atom(), atom()) -> boolean().
valid_name_method_conf(undefined, <<"GET">>) -> true;
valid_name_method_conf(_, <<"PUT">>) -> true;
valid_name_method_conf(Name, <<"DELETE">>) ->
    service_exists(Name);
valid_name_method_conf(Name, <<"POST">>) ->
    service_exists(Name);
valid_name_method_conf(Name, <<"GET">>) ->
    service_exists(Name).

-spec validate(binary(), proplists:proplist()) -> boolean().
validate(<<"POST">>, Body) ->
    validate_body(proplists:get_value(<<"update">>, Body, undefined), Body);
validate(<<"PUT">>, _) -> true.

-spec validate_body(binary(), propists:proplist()) -> boolean(). 
validate_body(<<"start">>, Body) ->
    proplists:is_defined(<<"pid">>, Body);
validate_body(<<"set_online">>, _) -> true;
validate_body(<<"set_offline">>, _) -> true;
validate_body(_, _) -> false.
