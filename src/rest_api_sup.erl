-module(rest_api_sup).

-behavior(supervisor).

-export([start_link/0, init/1, listen/0]).

-spec init(term()) -> {ok, {{supervisor:strategy(), non_neg_integer(), non_neg_integer()},
                            [supervisor:child_specs()]}}.
-define(SUPER(__Mod, __Args), {__Mod, {__Mod, start_link, __Args}, permanent, infinity, supervisor, [__Mod]}).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {}).

init(_Args) ->
    Ranch_Sup = ?SUPER(ranch_sup, []),
    Cowboy_Sup = ?SUPER(cowboy_sup, []),
    Begin_Listen = {?MODULE,  {?MODULE, listen, []}, transient, infinity, worker, [?MODULE]},
    Included_Apps = [Ranch_Sup, Cowboy_Sup, Begin_Listen],
    {ok, {{rest_for_one, 5, 60}, Included_Apps}}.
   
   
listen() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", rest_handler, []}
            ]}
        ]),
    {ok, _} = cowboy:start_http(http, 10, [{port, 8000}], [
        {env, [{dispatch, Dispatch}]}]). 
