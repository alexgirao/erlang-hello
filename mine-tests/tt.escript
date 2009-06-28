#!/usr/bin/env escript
%% -*- erlang -*-

-import(tt, [test_avg/4]).

main(_) ->
    make:files([tt]),
    R = timer:tc(erlang, length, [[]]),
    io:format("~p~n", [
        R
    ]),
    L = lists:seq(0, 10000),
    T1 = timer:tc(tt, lc1, [L]),
    T2 = timer:tc(tt, lc2, [L]),
    T3 = timer:tc(tt, lc3, [L]),
    io:format("~p~n", [
        [Time || {Time,_} <- [T1, T2, T3]]
    ]),
    test_avg(lists, seq, [1,10], 10000).
