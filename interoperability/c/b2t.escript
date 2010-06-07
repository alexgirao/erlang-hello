#!/usr/bin/env escript
%% -*- erlang -*-

show_binary_term(FN) ->
    {ok, Data} = file:read_file(FN),
    io:format("read ~p~n~p~n~p~n", [FN, Data, binary_to_term(Data)]).

main(_Args) ->
    show_binary_term("t2b.escript.0.bin"),
    show_binary_term("t2b.escript.1.bin"),
    show_binary_term("t2b.c.bin"),
    show_binary_term("108+107.bin"),
    show_binary_term("108+108+107.bin"),
    ok.
