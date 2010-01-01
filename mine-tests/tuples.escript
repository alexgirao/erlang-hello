#!/usr/bin/env escript
%% -*- erlang -*-

-import(erlang, [append_element/2]).

main(_) ->  % escript requires a main function (don't need to export explicitly)
    A = {alpha, bravo, charlie},    % literal tuples syntax
    B = {element(1, A), element(2, A), element(3, A)},   % tuples elements starts at 1
    C = A == B,
    D = setelement(1, A, "ALPHA"),
    E = tuple_size(A),
    F = append_element(A, delta),
    io:format("~p~n", [[A, B, C, D, E, F]]).
