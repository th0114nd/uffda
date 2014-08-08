-module(uffda_subscription).

-export([start_link/0,
         subscribe/3,
         unsubscribe/3,
         notify/2]).

-include("uffda.hrl").
-include("uffda_sub.hrl").

-spec start_link() -> {ok, pid()} | {error, {already_started, pid()}}.
start_link() ->
    gen_event:start_link({local, ?PUBLISH_MGR}).

-spec find_vars(sub_type(), address(), service_name()) -> {state(), {atom(), term()}}.
find_vars(Sub_Type, Return_Address, Service) ->
    Id = State = {Return_Address, Service},
    CB_Module = case Sub_Type of
        pid -> uffda_pid_publisher;
        sse -> uffda_sse_publisher
        end,
    {State, {CB_Module, Id}}.

-spec subscribe(sub_type(), address(), service_name()) -> ok.
subscribe(Sub_Type, Return_Address, Service) ->
    {State, Handler} = find_vars(Sub_Type, Return_Address, Service),
    gen_event:add_handler(?PUBLISH_MGR, Handler, State).

-spec unsubscribe(sub_type(), address(), service_name()) -> term().
unsubscribe(Sub_Type, Return_Address, Service) ->
    {_State, Handler} = find_vars(Sub_Type, Return_Address, Service),
    gen_event:delete_handler(?PUBLISH_MGR, Handler, {}).

-spec notify(service_name(), service_status()) -> ok.
notify(Service, Status) ->
    gen_event:notify(?PUBLISH_MGR, {publish, Service, Status}).
