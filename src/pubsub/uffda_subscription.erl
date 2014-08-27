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
find_vars(Sub_Type, Return_Address, Service)
  when is_atom(Sub_Type), is_atom(Service) ->
    {Updated_Return, CB_Module} = case Sub_Type of
        pid -> {Return_Address, uffda_pid_publisher};
        sse -> {self(), uffda_pid_publisher};
        email -> {Return_Address, uffda_email_publisher}
        end,
    Id = State = {Updated_Return, Service},
    {State, {CB_Module, Id}}.

-spec subscribe(sub_type(), address(), service_name()) -> ok.
subscribe(Sub_Type, Return_Address, Service)
  when is_atom(Sub_Type), is_atom(Service)->
    {State, Handler} = find_vars(Sub_Type, Return_Address, Service),
    gen_event:add_handler(?PUBLISH_MGR, Handler, State).

-spec unsubscribe(sub_type(), address(), service_name()) -> term().
unsubscribe(Sub_Type, Return_Address, Service)
  when is_atom(Sub_Type), is_atom(Service) ->
    {_State, Handler} = find_vars(Sub_Type, Return_Address, Service),
    gen_event:delete_handler(?PUBLISH_MGR, Handler, {}).

-spec notify(service_name(), service_status()) -> ok.
notify(Service, Status)
  when is_atom(Service), is_atom(Status) ->
    gen_event:notify(?PUBLISH_MGR, {publish, Service, Status}).
