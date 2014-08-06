-module(uffda_api_table).

-export([listen/0]).

listen() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {<<"/uffda-services">>, uffda_rest_handler, [which_services]},
            {<<"/uffda-services/hello">>, uffda_rest_handler, [hello_to_text]},
            {<<"/uffda-services/:name">>, uffda_rest_handler, [service_status]}
            ]}
        ]),
    {ok, _} = cowboy:start_http(http, 10, [{port, 8000}], [
        {env, [{dispatch, Dispatch}]}]). 
