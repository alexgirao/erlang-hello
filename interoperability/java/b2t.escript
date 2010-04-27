#!/usr/bin/env escript
%% -*- erlang -*-

main(_Args) ->
    FN = "t2b.java.bin",
    {ok, Data} = file:read_file(FN),
    io:format("read ~p~n~p~n", [FN, binary_to_term(Data)]).
