-module(uffda_root_sup).
-behavior(supervisor).

-export([start_link/0, init/1]).
-define(SUPER, ?MODULE).
-define(CHILD(__Mod), {__Mod, {__Mod, start_link, []},
                               permanent, 2000, worker, [__Mod]}).
-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?SUPER}, ?MODULE, {}).

-spec init({}) -> {ok, {{supervisor:strategy(), non_neg_integer(), non_neg_integer()},
                        [supervisor:child_spec()]}}.
init({}) ->
    Procs = [?CHILD(uffda_subscription),
             ?CHILD(uffda_registry_sup),
             ?CHILD(uffda_api_sup)],
    {ok, {{rest_for_one, 10, 10}, Procs}}.
