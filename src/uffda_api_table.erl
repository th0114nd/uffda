-module(uffda_api_table).

-export([listen/0]).

listen() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {<<"/uffda-services/[:name]">>, uffda_rest_handler, []}
            ]}
        ]),
    {ok, _} = cowboy:start_http(http, 10, [{port, 8000}], [
        {env, [{dispatch, Dispatch}]}]). 
