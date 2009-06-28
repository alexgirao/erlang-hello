-module(tt).
-export([
	 lc1/1,
	 lc2/1,
	 lc3/1,
	 test_avg/4
	]).

% lc1, lc2 and lc3 all do the same thing

lc1(L) -> % using if
    F = fun(X) ->
        if
            X == 1; X == 2 ->        % GuardSeq -> ...
                true;
            true ->
                false
        end
    end,
    [X || X <- L, F(X)].

lc2(L) -> % using match
    F = fun(X) ->
        case X of
            1 -> true;
            2 -> true;
            _ -> false
        end
    end,
    [X || X <- L, F(X)].

lc3(L) -> % using fun expressions/guards
    F = fun    
        (X) when X == 1; X == 2 -> true;
        (_) -> false
    end,
    [X || X <- L, F(X)].

%% reference: http://www.trapexit.org/Measuring_Function_Execution_Time

test_avg(M, F, A, N) when N > 0 ->
    L = test_loop(M, F, A, N, []),
    Length = length(L),
    Min = lists:min(L),
    Max = lists:max(L),
    Med = lists:nth(round((Length / 2)), lists:sort(L)),
    Avg = round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length),
    io:format("Range: ~b - ~b mics~n"
	      "Median: ~b mics~n"
	      "Average: ~b mics~n",
	      [Min, Max, Med, Avg]),
    Med.

test_loop(_M, _F, _A, 0, List) ->
    List;

test_loop(M, F, A, N, List) ->
    {T, _Result} = timer:tc(M, F, A),
    test_loop(M, F, A, N - 1, [T|List]).
