#!/usr/bin/env escript
%% -*- erlang -*-

% reference: http://literateprograms.org/Fisher-Yates_shuffle_(Erlang)

-define(debug_log_macros, true).
-include("$PWD/macro.hrl").

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

    ?log_msg1("~p ~p ~p", [{A, B, C}, {X, Y, Z}, random:uniform(16#ffffffff)]),

    L0 = lists:seq(1, 10),
    L1 = shuffle(L0),

    ?log_msg1("~p ~p", [L0, L1]),

    gen_event:stop(error_logger), % this flushes error_logger messages from escript processes
    io:format("~n"),
    ok.
