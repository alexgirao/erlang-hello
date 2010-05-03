#!/usr/bin/env escript
%% -*- erlang -*-

% two's-complement system or two's-complement arithmetic

% reference: http://en.wikipedia.org/wiki/Two's_complement

tc32(V) ->
    -((1 bsl 32) - V).

main(_) ->
    Unsigned = 3961800465,    % same as 0xec244711
    I32 = tc32(Unsigned),
    Unsigned = tc32(tc32(Unsigned)),
    I32 = tc32(tc32(I32)),
    io:format("~p = ~p~n", [Unsigned, I32]).
