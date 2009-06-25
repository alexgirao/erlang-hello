-module(tt).
-export([
	 lc1/1,
	 lc2/1,
	 lc3/1
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
