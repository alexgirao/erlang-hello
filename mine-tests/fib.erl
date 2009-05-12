#!/usr/bin/env escript
%% -*- erlang -*-

fib(Max) ->
    fib(Max, 0, 1).

fib(Max, I, J) when I + J < Max ->
    io:format("~p~n", [I]),
    fib(Max, J, I + J);
fib(Max, I, _) ->
    io:format("~p~n", [I]),
    ok.

main(_) ->
    fib(1000).
