-module(sys_sup).
-export([start_link/0, init/1]).

%%-----------------------------------------------------
%% API
%%-----------------------------------------------------

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {}). 

%%-----------------------------------------------------
%% Supervisor callbacks
%%-----------------------------------------------------

-define(SUPER(__Mod, __Args), {__Mod, {__Mod, start_link, __Args}, permanent, infinity, supervisor, [__Mod]}).

init({}) ->
    Procs = [?SUPER(Proc, []) || Proc <- [crypto_sup, ssl_sup]], 
    {ok, {{one_for_one, 10, 10}, Procs}}.
