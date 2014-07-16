-module(uffda_dsl).
-export([parse_from_file/1, run_program/1, create_sup_tree/1, extract_workers/1, unique_names/1]).
-export_type([program/0, sup_tree_spec/0]).
-include("uffda.hrl").

-type program() :: {{startup, sup_tree_spec()}, {actions, [action()]}}.
-type sup_tree_spec() :: union({leaf, wos()}, {node, super_desc(), [sup_tree_spec()]}).
-type wos() :: {worker, worker_desc()} | {supervisor, super_desc()}.

% In general, {Name, Module, Args}
-type sup_name() :: atom().
-type worker_name() :: atom().
-type super_desc() :: {sup_name(), ex_super, {}}.
% {Name, Module, starting_state}
-type worker_desc() :: {worker_name(), ex_worker, service_status()}.
-type action() :: {worker_name(), real_world_event()}.

-type reason() :: term().

-type real_world_event() :: go_up | go_down | crash.
-spec parse_from_file(string() | atom() | binary()) -> {ok, program()} | {error, reason()}. 
parse_from_file(File_Name) ->
    {ok, Raw} = file:consult(File_Name),
    [TreeSpec | Actions] = Raw,
    {ok, {{startup, TreeSpec}, {actions, Actions}}}.

-spec run_program(program()) -> ok | {error, reason()}.
run_program({{startup, Sup_Tree}, {actions, Actions}}) ->
    % Make sure that all the actions refer to workers present in the tree.
    true = ensure_workers_valid(Sup_Tree, Actions),
    ok = create_sup_tree(Sup_Tree),
    io:format("Actions: ~p", [Actions]),
    ok = execute_all(Actions).

-spec create_sup_tree(sup_tree_spec()) -> ok.
create_sup_tree({node, {supervisor, Parent}, Children}) ->
    {Name, Module, Args} = Parent,
    io:format("Name: ~p Mod: ~p Args ~p~n", [Name, Module, Args]),
    {ok, Sup_Ref} = supervisor:start_link({local, Name}, Module, Args),
    Child_Specs = [create_child_spec(Child) || {leaf, Child} <- Children],
    _ = [{ok, _} = supervisor:start_child(Sup_Ref, CS) || CS <- Child_Specs],
    ok;
create_sup_tree({leaf, Wos}) ->
    exit(not_implemented),
    case Wos of
        {super, {Name, Module, Args}} -> Module:start_link(Name, Args);
        {worker, {Name, Module, Start_State}} -> Module:start_link(Name, [Start_State])
    end.

-spec ensure_workers_valid(sup_tree_spec(), [action()]) -> boolean(). 
ensure_workers_valid(_Sup_Tree, _Actions) ->
    % Wait until further functionality added to implement.
    true.

-spec execute_all([action()]) -> ok.
execute_all([]) -> ok;
execute_all([Act | Acts]) ->
    {Worker, Event} = Act,
    Worker ! Event,
    execute_all(Acts).

-spec create_child_spec(wos()) -> supervisor:child_spec().
create_child_spec({worker, W}) -> create_worker_child_spec(W);
create_child_spec({super, S}) -> create_super_child_spec(S).

-spec create_worker_child_spec(worker_desc()) -> supervisor:child_spec().
create_worker_child_spec({Name, Module, Starting_State}) ->
    {Name, {Module, start_link, [Name, Starting_State]}, transient, 1000, worker, [Module]}.

-spec create_super_child_spec(super_desc()) -> supervisor:child_spec().
create_super_child_spec({Name, Module, Args}) ->
    {Name, {Module, start_link, Args}, transient, 1000, supervisor, [Module]}.

-spec extract_workers(sup_tree_spec()) -> [atom()].
extract_workers({leaf, {supervisor, _}}) -> [];
extract_workers({leaf, {worker, {Name, _, _}}}) -> [Name];
extract_workers({node, _, Children}) ->  
    lists:append([extract_workers(Child) || Child <- Children]).

extract_names({leaf, {_, {Name, _, _}}}) -> [Name];
extract_names({node, {Name, _, _}, Children}) ->
    [Name | lists:append([extract_names(Child) || Child <- Children])];
extract_names(Other) ->
    ct:log("found other: ~p", [Other]),
    [].
    
-spec unique_names(sup_tree_spec()) -> boolean.
unique_names(Tree) ->
    All = extract_names(Tree),
    length(All) == sets:size(sets:from_list(All)).
