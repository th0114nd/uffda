-module(uffda_publisher).

-behaviour(gen_event).
-author('tholland@tigertext.com').
-include("uffda.hrl").

-export([init/1, handle_event/2, handle_call/2]).
-export([handle_info/2, code_change/3, terminate/2]).

-callback send(Event_Type :: event(), AddSerSta :: ass()) -> ok.

-spec init(state()) -> {ok, state()}.
init({Module, Ass} = Mass) ->
    Module:send(subscription_starting, Ass),
    {ok, Mass}.

-spec handle_event(event(), state()) -> {ok, state()}.
handle_event({publish, Service, Status}, {Module, #ass{service = Service} = Ass}) ->
    New_Ass = Ass#ass{status = Status},
    Module:send(updating, New_Ass),
    {ok, {Module, New_Ass}}; 
handle_event(_, Mass) ->
    {ok, Mass}.

-spec handle_call(call(), state()) -> {ok, {module(), address(), service_name()}, {module(), ass()}}.
handle_call(report_subscriber, {Module, #ass{address = Address, service = Service}} = Mass) ->
    Subscriber = {Module, Address, Service},
    {ok, Subscriber, Mass}.

-spec handle_info(any(), state()) -> {ok, state()}.
handle_info(_Msg, Mass) ->
    {ok, Mass}.

-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, Mass, _Extra) ->
    {ok, Mass}.

-spec terminate({}, {module(), ass()}) -> ok.
terminate({}, {Module, Ass}) ->
    Module:send(unsubscribing, Ass), 
    ok.
