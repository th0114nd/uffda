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
    Expect = fun(Msg) ->
                receive
                    Msg -> ok;
                    Other -> ct:log("Unexpected:~n~p~nvs~n~p", [Msg, Other]),
                             "Unexpected"
                 after
                    100 -> "Msg not received"
                 end
             end, 
    ok = uffda_subscription:subscribe(pid, self(), foo),
    ok = Expect({subscription_starting, {foo, nonexistent}}), 
    ok = uffda_client:register_service(foo),
    ok = Expect({updating, {foo, registered}}),
    ok = uffda_client:set_service_online(foo),
    ok = Expect({updating, {foo, up}}),
    ok = uffda_client:set_service_offline(foo),
    ok = Expect({updating, {foo, down}}),
    ok = uffda_subscription:unsubscribe(pid, self(), foo),
    ok = Expect({unsubscribing, {foo, down}}).

