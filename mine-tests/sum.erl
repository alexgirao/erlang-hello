#!/usr/bin/env escript
%% -*- erlang -*-

% sum_bad/1: non tail call
sum_bad([H|T]) -> H + sum_bad(T);
sum_bad([]) -> 0.

% sum_tc/1/2: tail call
sum_tc(L) -> sum_tc(L, 0).

sum_tc([], Acc) -> Acc;
sum_tc([H|T], Acc) -> sum_tc(T, Acc + H).

main(_) ->
    L = lists:seq(1,100),
    S1 = sum_bad(L),
    S2 = sum_tc(L),
    io:format("~p ~p~n", [S1, S2]).
