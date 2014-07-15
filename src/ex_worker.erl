-module(ex_worker).
-export([start_link/2, startup/2, service_loop/1]).

-spec start_link(atom(), any()) -> {ok, pid()}.
start_link(Name, _Args) ->
    Pid = spawn(fun() -> startup(Name, self()) end),
    {ok, Pid}.

    
-spec startup(atom(), pid()) -> term().
%% @hidden
%% @doc
%%   Register and starting up actions for a local test service.
%% @end
startup(Name, Caller) ->
    true = register(Name, self()),
    uffda_client:register_service(Name),
    Caller ! {ok, Name},
    ok = uffda_client:starting_service(Name, self()),
    service_loop(Name).

-spec service_loop(atom()) -> term().
%% @hidden
%% @doc
%%   Receive loop for local test service to induce service events.
%% @end
service_loop(Name) ->
    _  = receive
        {whoami, Pid} -> Pid ! Name;
        crash   -> exit(kill);
        unregister -> uffda_client:unregister_service(Name);
        online    -> uffda_client:set_service_online(Name);
        offline  -> uffda_client:set_service_offline(Name);
        {state, Pid} -> Pid ! uffda_client:service_status(Name)
    end,
    service_loop(Name).
