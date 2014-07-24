-module(uffda_subscription).

-behaviour(gen_server).

-export([start_link/0,
         ping/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).
-define(SUBSCRIBER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SUBSCRIBER}, ?MODULE, [], []).

ping() ->
    gen_server:call(ping, ?SUBSCRIBER).

init([]) ->
    {ok, #state{}}.

handle_call(ping, _From, State) ->
    {reply, pong, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
