-module(f).
-export([main/0, flatten_r/1, flatten/1, flatten/2]).
-include_lib("eunit/include/eunit.hrl").

% erlc f.erl && erl -noshell -s f main -s init stop
    
% flatten_r/1: recursive implementation
% note: recursive functions has stack size limitations

flatten_r([])->
    [];
flatten_r([[H|T]|T2]) ->
    flatten([H|T ++ T2]);
flatten_r([H|T]) ->
    [H | flatten(T)].

% flatten/1: tail recursive implementation (uses accumulator)
% note: tail recursive functions reuses stack since all relevant data is passed via accumulator

flatten(L) ->
    % io:format("flattening ~p~n", [L]),
    flatten(L, []).

flatten([[H]], Acc) ->        % [H] = [H|[]]
    % io:format("case 1~n"),
    flatten([H], Acc);
flatten([[H|T]], Acc) ->
    % io:format("case 2~n"),
    flatten([H|T], Acc);
flatten([[H]|T], Acc) ->
    % io:format("case 3~n"),
    flatten([H|T], Acc);
flatten([[H|T]|T2], Acc) ->  % case 1, 2 and 3 implementations can be omited (as it is in flatten_r/1)
    % io:format("case 4~n"),
    flatten([H|T ++ T2], Acc);
flatten([H|T], Acc) ->
    % we are at a leaf at H, we can finally accumulate
    flatten(T, [H|Acc]);
flatten([], Acc) ->
    % no more items, reverse accumulator
    lists:reverse(Acc).

main() ->
    Tests = [
	     {[], []},
	     {[1], [1]},
	     {[[1]], [1]},
	     {[[[1]]], [1]},
	     {[[[[1]]]], [1]},
	     {[[[[1], 2]]], [1, 2]},
	     {[[[[1, 2]]]], [1, 2]},
	     {[[[[1, 2, 3]]]], [1, 2, 3]},
	     {[[[[1, 2], 3]]], [1, 2, 3]},
	     {[[[[1, 2]], 3]], [1, 2, 3]},
	     {[[[[1, 2]]], 3], [1, 2, 3]},
	     {[[[[1], 2], 3], 4], [1, 2, 3, 4]},
	     {[1, 2, 3], [1, 2, 3]},
	     {[1, [2, [3, [4, [5, [6]]]]]], [1, 2, 3, 4, 5, 6]}
	    ],
    [?assert(flatten(A) == B) || {A, B} <- Tests],        % see lc.erl for more information about list comprehension syntax
    [?assert(flatten_r(A) == B) || {A, B} <- Tests].
