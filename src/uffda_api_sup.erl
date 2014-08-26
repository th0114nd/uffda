-module(uffda_api_sup).

-behavior(supervisor).

-export([start_link/0, init/1]).

-define(SUPER(__Mod), {__Mod, {__Mod, start_link, []}, transient, infinity, supervisor, [__Mod]}).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {}).

-type restart() :: {supervisor:strategy(), non_neg_integer(), non_neg_integer()}.
-spec init({}) -> {'ok',{restart(), [?SUPER(module())]}}.
init({}) ->
    Ranch_Sup = ?SUPER(ranch_sup),
    Cowboy_Sup = ?SUPER(cowboy_sup),
    Apps = [Ranch_Sup, Cowboy_Sup],
    {ok, {{rest_for_one, 5, 60}, Apps}}.
