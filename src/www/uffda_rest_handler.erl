-module(uffda_rest_handler).
-author('pierce.mt92@gmail.com').

%% Cowboy REST callbacks
-export([init/3,
         rest_init/2,
         allowed_methods/2,
         resource_exists/2,
         delete_resource/2,
         content_types_accepted/2,
         content_types_provided/2,
         terminate/3]).

-export([accept_form/2,
         dispatch_on_method/2]).

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

allowed_methods(Req, #state{name = Name} = State) ->
    {if
        Name /= undefined -> [<<"GET">>, <<"DELETE">>, <<"PUT">>, <<"POST">>];
        true -> [<<"GET">>]
     end, Req, State}.

-spec resource_exists(req(), #state{}) -> {boolean(), req(), #state{}}.
resource_exists(Req, #state{name = Name, method = Method} = State) ->
    {case {Name, Method} of
        {undefined, <<"GET">>} -> true;
        {_, <<"PUT">>} -> true;
        {Name, <<"DELETE">>} -> service_exists(Name);
        {Name, <<"POST">>} -> service_exists(Name);
        {Name, <<"GET">>} -> service_exists(Name)
       end,Req, State}.

-spec content_types_provided(req(), #state{}) -> {[{atom() | binary(), atom()}], req(), #state{}}.
content_types_accepted(Req, #state{} = State) ->
    {[
        {<<"application/x-www-form-urlencoded">>, accept_form},
        {'*', dispatch_on_method}
    ], Req, State}.

-spec content_types_accepted(req(), #state{}) -> {[{binary(), atom()}], req(), #state{}}.
content_types_provided(Req, State) ->
    {[
        {<<"text/plain">>, dispatch_on_method}
    ], Req, State}.

-spec delete_resource(req(), #state{}) -> {boolean(), req(), #state{}}.
delete_resource(Req, #state{name = Name} = State) ->
    {ok == uffda_client:unregister_service(Name) andalso
     not service_exists(Name), Req, State}.

-spec terminate(any(), req(), #state{}) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.

%%------------------------------------------------------------------------------------
%% IO callbacks
%%------------------------------------------------------------------------------------

-spec accept_form(req(), #state{}) -> {boolean(), req(), #state{}}.
accept_form(Req, #state{name = Name} = State) ->
    {ok, Body, Req2} = cowboy_req:body_qs(Req),
    {Name, Req3} = cowboy_req:binding(name, Req2),
    Update = proplists:get_value(<<"update">>, Body),
    {try
        {Func, Status} = case Update of
            <<"start">> -> {starting_service, starting_up};
            <<"set_online">> -> {set_service_online, up};
            <<"set_offline">> -> {set_service_offline, down}
            end,
        ok = uffda_client:Func(Name),
        uffda_client:service_status(Name) =:= Status
    catch
        _:_ -> false
    end, Req3, State}.

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

-spec seq_to_iodata([atom()]) -> iodata().
seq_to_iodata([]) -> "";
seq_to_iodata([A | T]) when is_atom(A) -> [atom_to_list(A), $\n | seq_to_iodata(T)].

-spec dispatch_on_method(req(), #state{}) -> {iodata() | boolean(), req(), #state{}}.
dispatch_on_method(Req, #state{name = Name, method = Method} = State) ->
    {case {Name, Method} of
        {undefined, <<"GET">>} -> seq_to_iodata(uffda_client:which_services());
        {Name, <<"GET">>} -> seq_to_iodata([uffda_client:service_status(Name)]);
        {Name, <<"PUT">>} ->
            case uffda_client:register_service(Name) of
                ok -> service_exists(Name);
                {error, already_started} -> false;
                {error, {not_started, _}} -> exit(internal_server_error)
            end
     end, Req, State}.

-spec service_exists(any()) -> boolean().
service_exists(Name) -> lists:member(Name, uffda_client:which_services()).
