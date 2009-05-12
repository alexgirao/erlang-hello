#!/usr/bin/env escript
%% -*- erlang -*-

%% list comprehensions

main(_) ->
    L = lists:seq(0,9),

    L0 = [X || X <- L, X rem 2 == 0],
    io:format("~p~n", [L0]),

    L1 = [X + 1000 || X <- L, X == 1 orelse X == 2],
    io:format("~p~n", [L1]),

    L2 = [X * 1000 || X <- L, X rem 2 /= 0 andalso X >= 5],    % /= is not equal operator
    io:format("~p~n", [L2]),

    ok.
