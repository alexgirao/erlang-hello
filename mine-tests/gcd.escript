#!/usr/bin/env escript
%% -*- erlang -*-

gcd(X, Y) when X > 0 andalso Y > 0 ->
    gcd0(Y rem X, X).

gcd0(0, Y) ->
    Y;

gcd0(X, Y) ->
    gcd0(Y rem X, X).

main(_) ->
    lists:foreach(fun (I) ->
			  GCD = gcd(I, 32),
			  io:format("~2.10. b / ~2.10. b = ~f (GCD=~b)~n", [trunc(I / GCD), trunc(32 / GCD), I/32, GCD])
		  end, lists:seq(1, 32-1)),
    ok.
