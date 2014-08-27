%% @doc
%%   Testing the publication/subscription model
%%   <ol>
%%   </ol>
%% @end
-module(uffda_pubsub_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([easy/1]).

-include("uffda_common_test.hrl").

-spec all() -> [module()].
all() -> [easy].

-type config() :: proplists:proplist().

-spec init_per_testcase(module(), config()) -> config().
init_per_testcase(_TestCase, Config) ->
    ok = uffda:start(),
    Config.

-spec end_per_testcase(module(), config()) -> ok.
end_per_testcase(_TestCase, _Config) ->
    uffda:stop().

-spec easy(term()) -> ok.
%% @doc
%%   Simple testing of register, online, offline and status reporting.
%% @end
easy(_Config) ->
    ct:comment("Basic functionality"),
    ok = uffda_subscription:subscribe(pid, self(), foo),
    ok = uffda_client:register_service(foo),
    Expect = fun(Msg) ->
                receive
                    Msg -> ok
                 after
                    100 -> "Msg not received"
                 end
             end, 
    ok = Expect({update, {foo, registered}}),
    ok = uffda_client:set_service_online(foo),
    ok = Expect({update, {foo, up}}),
    ok = uffda_client:set_service_offline(foo),
    ok = Expect({update, {foo, down}}).

