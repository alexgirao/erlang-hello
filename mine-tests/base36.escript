#!/usr/bin/env escript
%% -*- erlang -*-

val(B, C) ->
    {V, C} = lists:keyfind(C, 2, B),
    V.

char(B, V) ->
    {V, C} = lists:keyfind(V, 1, B),
    C.

v(B, L) ->
    v(B, L, 0).

v(B, [H|T], A) ->
    v(B, T, A * 36 + val(B, H));

v(_B, [], A) ->
    A.

s(B, V) ->
    s(B, V, []).

s(_B, 0, A) ->
    A;

s(B, V, A) ->
    s(B, V div 36, [char(B, V rem 36) | A]).

main(_Args) ->
    B = [{I-$0, I} || I <- lists:seq($0, $9)] ++ [{I-$A+10, I} || I <- lists:seq($A, $Z)],
    io:format("val($A)         = ~p~n", [val(B, $A)]),
    io:format("[char(0)]       = ~p~n", [[char(B, 0)]]),
    io:format("v(\"100\")        = ~p~n", [v(B, "100")]),
    io:format("36#100          = ~p~n", [36#100]),  % erlang already supports base 36! discovered after
    io:format("s(36#100)       = ~p~n", [s(B, 36#100)]),
    io:format("s(16#FFFFFFFF)  = ~p~n", [s(B, 16#FFFFFFFF)]),
    io:format("s(36#ZZZZZZ)    = ~p~n", [s(B, 36#ZZZZZZ)]),
    io:format("36#ZZZZZZ       = ~p~n", [36#ZZZZZZ]),
    io:format("v(\"ZZZZZZ\")     = ~p~n", [v(B, "ZZZZZZ")]),
    ok.
