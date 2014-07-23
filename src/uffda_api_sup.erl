-module(uffda_api_sup).

-behavior(supervisor).

-export([start_link/0, init/1]).

-spec init(term()) -> {ok, {{supervisor:strategy(), non_neg_integer(), non_neg_integer()},
                            [supervisor:child_specs()]}}.
-define(SUPER(__Mod, __Args), {__Mod, {__Mod, start_link, __Args}, transient, infinity, supervisor, [__Mod]}).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {}).

init(_Args) ->
    Ranch_Sup = ?SUPER(ranch_sup, []),
    Cowboy_Sup = ?SUPER(cowboy_sup, []),
    Apps_N_Children = [Ranch_Sup, Cowboy_Sup],
    {ok, {{rest_for_one, 5, 60}, Apps_N_Children}}.
