-module(uffda_pid_publisher).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2]).
-export([handle_info/2, code_change/3, terminate/2]).

-include("uffda.hrl").
-include("uffda_sub.hrl").

-spec init(state()) -> {ok, state()}.
init(State) ->
    {ok, State}.

-spec handle_event(event(), state()) -> {ok, state()}.
handle_event({publish, Service, Status}, State = {Return_Address, Service}) ->
    Return_Address ! {update, {Service, Status}},
    {ok, State};
handle_event(_, State) ->
    {ok, State}.

-spec handle_call(call(), state()) -> {ok, state(), state()}.
handle_call(report_subscriber, State) ->
    {ok, State, State}.

%% Required behaviour callbacks, unused.
-spec handle_info(any(), state()) -> {ok, state()}.
handle_info(_Msg, State) ->
    {ok, State}.

-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec terminate(term(), state()) -> ok.
terminate({}, _State) ->
    ok.
