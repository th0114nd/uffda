-module(uffda_sse_publisher).
-behaviour(gen_event).
-export([init/1, handle_event/2, handle_call/2, terminate/2]).
-export([handle_info/2, code_change/3]).
-include("uffda.hrl").
-include("uffda_sub.hrl").

-spec init(state()) -> {ok, state()}.
init(State) ->
    % make the sse connection to the return address.
    {ok, State}.

-spec handle_event({publish, service_status()}, state()) -> {ok, state()}.
handle_event({publish, Service, _Status}, State = {_Return_Address, Service}) ->
    % Send the status back to the return_address
    {ok, State};
handle_event(_, State) ->
    {ok, State}.

-spec handle_call(call(), state()) -> {ok, state(), state()}.
handle_call(report_subscriber, State) ->
    {ok, State, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Arg, _State) ->
    % Close the SSE connection.
    ok.

%% Required behaviour callbacks, unused.
-spec handle_info(any(), state()) -> {ok, state()}.
handle_info(_Msg, State) ->
    {ok, State}.

-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
