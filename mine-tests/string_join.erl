-module(string_join).
-export([my_string_join/2, join/2]).

% reference: http://www.trapexit.org/String_join_with

my_string_join(Items, Sep) ->
    lists:flatten(lists:reverse(my_string_join1(Items, Sep, []))).

my_string_join1([Head | []], _Sep, Acc) ->
    [Head | Acc];

my_string_join1([Head | Tail], Sep, Acc) ->
    my_string_join1(Tail, Sep, [Sep, Head | Acc]).

% join

join(L=[H|T], S) ->
    join(T, S, [H]).

join([], _S, Acc) ->
    lists:flatten(lists:reverse(Acc));

join([H | T], S, Acc) ->
    join(T, S, [H, S | Acc]).
