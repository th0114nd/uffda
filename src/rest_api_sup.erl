-module(rest_api_sup).

-behavior(supervisor).

-export([start_link/0, init/1]).

-spec init(term()) -> {ok, {{supervisor:strategy(), non_neg_integer(), non_neg_integer()},
                            [supervisor:child_specs()]}}.
-define(SUPER(__Mod, __Args), {__Mod, {__Mod, start_link, __Args}, permanent, infinity, supervisor, [__Mod]}).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {}).

init(_Args) ->
    Ranch_Sup = ?SUPER(ranch_sup, []),
    Cowboy_Sup = ?SUPER(cowboy_sup, []),
    Begin_Listen = {uffda_api_table,  {uffda_api_table, listen, []}, transient, infinity, worker, [uffda_api_table]},
	Sys_Sup = ?SUPER(sys_sup, []),
    Apps_N_Children = [Ranch_Sup, Cowboy_Sup, Begin_Listen, Sys_sup],
    {ok, {{rest_for_one, 5, 60}, Apps_N_Children}}.
