-module(uffda_service).
-export([start/4, stop/2]).

-callback init(Args :: list(term())) -> ok | {error, Reason :: term()}.
-callback mainloop(Args :: list(term())) -> ok | {error, Reason :: term()}.
-callback terminate() -> ok | {error, Reason :: term()}.

start(Service_Name, Service_Module, Init_Args, Loop_Args) ->
    uffda_client:register_service(Service_Name),
    {ok, Pid} = Service_Module:init(Init_Args),
    _ = uffda_client:starting_up(Service_Module, Pid),
    Service_Module:mainloop(Loop_Args).
   
stop(Service_Name, Service_Module) ->
    ok = Service_Module:terminate(),
    uffda_client:unregister_service(Service_Name).