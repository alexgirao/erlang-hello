#!/usr/bin/env escript
%% -*- erlang -*-

first_byte(A) when is_binary(A) ->
    element(1, split_binary(A, 1)).

main(_) ->
    S = element(2, split_binary(term_to_binary([1, 2, 3]), 1)), % exclude version (byte 131)
    L0 = element(2, split_binary(term_to_binary([1000, 2000, 3000]), 1)), % exclude version (byte 131)
    L = element(1, split_binary(L0, size(L0) - 1)),  % exclude tail (byte 106)

    <<107>> = first_byte(S),  % short list/string
    <<108>> = first_byte(L),  % list

    C = binary_to_term(iolist_to_binary([131, L, S])), % concatenate the lists
    C = [1000,2000,3000,1,2,3],

    D = binary_to_term(iolist_to_binary([131, S, L])), % won't work
    D = [1,2,3],

    ok.
