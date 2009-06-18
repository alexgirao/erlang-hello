#!/usr/bin/env escript
%% -*- erlang -*-

%% list comprehensions

main(_) ->
    L = lists:seq(0, 9),
    io:format("~p~n", [L]),

    io:format("~p~n", [
        [X * 10 || X <- L]
    ]),

    io:format("~p~n", [
        [X || X <- L, X rem 2 == 0]
    ]),

    io:format("~p~n", [
        [X || X <- L, X == 1 orelse X == 2]
    ]),

    io:format("~p~n", [
        [X || X <- L, X rem 2 /= 0 andalso X >= 5]
    ]),

    % , (comma) also works as logical and

    io:format("~p~n", [
        [X || X <- L, X rem 2 /= 0, X >= 5]
    ]),

    ok.
