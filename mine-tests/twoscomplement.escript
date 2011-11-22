#!/usr/bin/env escript
%% -*- erlang -*-

% two's-complement system or two's-complement arithmetic

% reference: http://en.wikipedia.org/wiki/Two's_complement

tc32(V) ->
    -(V + 1).

main(_) ->
    Unsigned = 3961800465,
    Unsigned = 16#ec244711,
    -3961800466 = tc32(Unsigned),
    Unsigned = tc32(tc32(Unsigned)),
    -3961800466 = tc32(tc32(tc32(Unsigned))),
    ok.
