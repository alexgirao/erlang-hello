#!/usr/bin/env escript
%% -*- erlang -*-

-import(erlang, [append_element/2]).

main(_) ->
    A = {alpha, bravo, charlie},
    B = {element(1, A), element(2, A), element(3, A)},
    C = A == B,
    D = setelement(1, A, "ALPHA"),
    E = tuple_size(A),
    F = append_element(A, delta),
    io:format("~p~n", [[A, B, C, D, E, F]]).
