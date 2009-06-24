#!/usr/bin/env escript
%% -*- erlang -*-

%% list comprehensions
%% see also: erl5.7.1/doc/reference_manual/expressions.html#6.24

main(_) ->
    L = lists:seq(0, 9),
    io:format("~p~n", [L]),

    io:format("~p~n", [
        [X * 10 || X <- L]
    ]),

    io:format("~p~n", [
        [X || X <- L, X rem 2 == 0]
    ]),

    io:format("~p~n", [
        [X || X <- L, X == 1 orelse X == 2]             % orelse use short-circuit
    ]),

    io:format("~p~n", [
        [X || X <- L, X rem 2 /= 0 andalso X >= 5]      % andalso use short-circuit
    ]),

    % , (comma) also works as "and" (filter is a GuardExpr?)

    io:format("~p~n", [
        [X || X <- L, X rem 2 /= 0, X >= 5]     
    ]),

    % using if

    F = fun(X) ->
        if
            X == 1; X == 2 ->        % GuardSeq -> ...
                true;
            true ->
                false
        end
    end,

    io:format("~p~n", [
        %[X || X <- L, X rem 2 /= 0; X >= 5]     % won't work, filter is GuardExpr (as opposed to GuardSeq)
        [X || X <- L, F(X)]
    ]),

    % using match

    F2 = fun(X) ->
        case X of
            1 -> true;
            2 -> true;
            _ -> false
        end
    end,

    io:format("~p~n", [
        [X || X <- L, F2(X)]

    ]),

    % using fun expression (fun + guards)

    F3 = fun    % using guards
        (X) when X == 1; X == 2 -> true;
        (_) -> false
    end,

    io:format("~p~n", [
        [X || X <- L, F3(X)]
    ]),

    ok.
