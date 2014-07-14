
-export[startup/2, service_loop/1]).
-spec startup(atom(), pid()) -> term().
%% @hidden
%% @doc
%%   Register and starting up actions for a local test service.
%% @end
startup(Name, Caller) ->
    uffda_client:register_service(Name),
    Caller ! {ok, Name},
    uffda_client:starting_service(Name, self()),
    service_loop(Name).

-spec service_loop(atom()) -> term().
%% @hidden
%% @doc
%%   Receive loop for local test service to induce service events.
%% @end
service_loop(Name) ->
    receive
        die   -> exit(kill);
        up    -> uffda_client:set_service_online(Name);
        down  -> uffda_client:set_service_offline(Name);
        {state, Pid} -> Pid ! uffda_client:service_status(Name)
    end,
    service_loop(Name).
