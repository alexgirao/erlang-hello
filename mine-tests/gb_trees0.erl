-module(gb_trees0).
-export([main/0]).

% erlc gb_trees0.erl && erl -noshell -s gb_trees0 main -s init stop

t0_loop_iter([]) ->
    ok;
t0_loop_iter(I)->
    {K, V, I2} = gb_trees:next(I),
    io:format("~p = ~p~n", [K, V]),
    t0_loop_iter(I2)
    .

t0() ->
    T0 = gb_trees:empty(),

    % enter = insert or update
    % insert = insert or error
    % update = update or error
    % enter, inser, update = same arguments

    T1 = gb_trees:enter(1, one, T0),
    T2 = gb_trees:enter(2, two, T1),
    T3 = gb_trees:enter(3, three, T2),
    T = gb_trees:enter(3, charlie, T3),

    io:format("keys: ~p~n", [gb_trees:keys(T)]),
    io:format("values: ~p~n", [gb_trees:values(T)]),

    % according to manual, using gb_trees:iterator/1 is slightly
    % slower than gb_trees:to_list/1 with the advantage of memory
    % efficiency

    t0_loop_iter(gb_trees:iterator(T)),

    % get(Key, Tree) -> Val
    % lookup(Key, Tree) -> {value, Val} | none

    io:format("get(1,T) = ~p~n", [gb_trees:get(1, T)]),
    io:format("lookup(2,T) = ~p~n", [gb_trees:lookup(2, T)]),
    io:format("lookup(9,T) = ~p~n", [gb_trees:lookup(9, T)]),

    % other very useful functions
    %
    %  delete(Key, Tree1) -> Tree2
    %  delete_any(Key, Tree1) -> Tree2
    %  is_defined(Key, Tree) -> bool()
    %  is_empty(Tree) -> bool()
    %  size(Tree) -> int()
    %

    % to_list(Tree) -> [{Key, Val}]
    % from_orddict(List) -> Tree

    t0_loop_iter(gb_trees:iterator(gb_trees:from_orddict(gb_trees:to_list(T)))),

    ok.

main() ->
    t0(),
    ok.
