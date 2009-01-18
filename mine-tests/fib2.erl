#!/usr/bin/env escript

%% -*- erlang -*-

fib(Max) ->
    fib(Max, 0, 0, 1).

fib(Max, Acc, I, J) when I rem 2 == 1 ->    % I is odd, don't sum
    fib(Max, Acc, J, I + J);
fib(Max, Acc, I, J) when Acc + I < Max ->   % I is even
    fib(Max, Acc + I, J, I + J);
fib(_Max, Acc, _I, _J) ->
    Acc.

main(_) ->
    io:format("~p~n", [fib(1000000)]).
