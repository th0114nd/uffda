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
    Allowed_Methods = 
        case State#state.name of
            undefined -> [<<"GET">>];
            _ -> [<<"GET">>, <<"DELETE">>, <<"PUT">>, <<"POST">>]
        end,
    {Allowed_Methods, Req, State}.

resource_exists(Req, State) ->
    {valid_name_method_conf(State), Req, State}. 

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
    dispatch_on_method(State),
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
    Response = dispatch_on_method(State),
    {Response, Req, State}.

accept_app(Req, State) ->
    {ok, Body, Req2} = cowboy_req:body_qs(Req),
    Result = validate(State#state.method, Body) andalso
             dispatch_on_method_with_body(State, Body),
    {Result, Req2, State}.

output_plain_text(Req, State) ->
    Response = dispatch_on_method(State),
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

-spec dispatch_on_method(#state{}) -> binary() | boolean(). 
dispatch_on_method(#state{name = undefined, method = <<"GET">>}) ->
    which_services();
dispatch_on_method(#state{name = Name, method = <<"GET">>}) ->
    service_status(Name);
dispatch_on_method(#state{name = Name, method = <<"DELETE">>}) ->
    delete_service(Name);
dispatch_on_method(#state{name = Name, method = <<"PUT">>}) ->
    add_service(Name, undefined);
dispatch_on_method(#state{method = <<"POST">>}) -> false.

-spec dispatch_on_method_with_body(#state{}, proplists:proplist()) -> boolean().
dispatch_on_method_with_body(#state{name = Name, method = <<"POST">>}, Body) ->
    update_service_status(Name, proplists:get_value(<<"update">>, Body), Body);
dispatch_on_method_with_body(#state{name = Name, method = <<"PUT">>}, Body) ->
    add_service(Name, proplists:get_value(<<"timeout">>, Body, undefined)).

-spec valid_name_method_conf(#state{}) -> boolean().
valid_name_method_conf(#state{name = Name, method = <<"DELETE">>}) ->
    service_exists(Name);
valid_name_method_conf(#state{name = Name, method = <<"POST">>}) ->
    service_exists(Name);
valid_name_method_conf(#state{name = undefined, method = <<"GET">>}) ->
    true;
valid_name_method_conf(#state{name = Name, method = <<"GET">>}) ->
    service_exists(Name);
valid_name_method_conf(#state{method = <<"PUT">>}) ->
    true.

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
