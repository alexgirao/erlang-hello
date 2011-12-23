#!/usr/bin/env escript
%% -*- erlang -*-

% reference: http://literateprograms.org/Fisher-Yates_shuffle_(Erlang)

% assumes big-endian
binary_to_integer(B) when is_binary(B) ->
    lists:foldl(fun (V, Acc) -> (Acc bsl 8) + V end, 0, binary_to_list(B)).

rand32bit() ->
    binary_to_integer(crypto:rand_bytes(4)).

shuffle(List) ->
    shuffle(List, []).

shuffle([], Acc) ->
    Acc;

shuffle(List, Acc) ->
    {Leading, [H | T]} = lists:split(random:uniform(length(List)) - 1, List),
    shuffle(Leading ++ T, [H | Acc]).

main(_Args) ->
    crypto:start(),  % this seems to be redundant

    {A, B, C} = now(),

    X = rand32bit(),
    Y = rand32bit(),
    Z = rand32bit(),
    
    random:seed(A+X, B+Y, C+Z),

    L0 = lists:sort(lists:sublist(shuffle(lists:seq(1, 60)), 6)),

    io:format("~w~n", [L0]),

    ok.
