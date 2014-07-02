-module(uffda_proper_SUITE).
-vsn('').

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([basic/1]).

all() -> [basic].

init_per_suite(Config) -> Config.
end_per_suite(Config) -> Config.

%% Testcases.
basic(_) -> ok.
