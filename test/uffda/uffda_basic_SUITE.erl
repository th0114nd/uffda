-module(uffda_basic_SUITE).
-vsn('').

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
        easy/1
        ]).

-include("uffda_common_test.hrl").

all() -> [
    easy
    ].

init_per_suite(Config) -> Config.
end_per_suite(Config) -> Config.

-define(TM, sr_client).

-spec easy(config()) -> ok.
easy(_Config) -> ok.
