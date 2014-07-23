-module(uffda_rest).
-export([begin_http/0, end_http/0]).

-spec begin_http() -> ok | {error, term}.
begin_http() ->
    Dispatch = cowboy_router:compile([{'_',
        uffda_api_table:dt()
    }]),
    {ok, _Pid} = cowboy:start_http(uffda_cowboy_http, 10, [{port, 8000}], [
        {env, [{dispatch, Dispatch}]}]).

-spec end_http() -> ok.
end_http() ->
    cowboy:stop_listener(uffda_cowboy_http).
