#!/usr/bin/env escript
%% -*- erlang, coding: utf-8 -*-

% 'case' is the erlang equivalent to ocaml 'match'

test_case0(V) ->
    case V of
        1 ->
            {number, "one"};
        {test_oddity, N} when N rem 2 == 0 ->
            {number_even, N};
        {test_oddity, N} when N rem 2 /= 0 ->
            {number_odd, N};
        Any ->        % match any
            {unknown, Any}
    end.

pl([H|T]) ->
    io:format("~p~n", [H]),
    pl(T);
pl([]) ->
    ok.

main(_) ->
    R = [
        test_case0(1),
        test_case0(999),
        test_case0(something),
        test_case0("something"),
        test_case0({test_oddity, 0}),
        test_case0({test_oddity, 1}),
        test_case0({test_oddity, 2}),
        test_case0({test_oddity, 3}),
        test_case0({test_oddity, 4})
    ],
    pl(R).
