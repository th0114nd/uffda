-module(ex_super).
-behavior(supervisor).

-export([start_link/1, init/1]).

-spec start_link(atom()) -> {ok, pid()}.
start_link(Name) ->
    ct:log("Registered: ~p", [registered()]),
    supervisor:start_link({local, Name}, ?MODULE, {}).
    
-spec init({}) -> {ok, {{supervisor:strategy(), non_neg_integer(),non_neg_integer()},
                        [supervisor:child_spec()]}}.  
init({}) ->
    {ok, {{one_for_one, 10, 10}, []}}.
