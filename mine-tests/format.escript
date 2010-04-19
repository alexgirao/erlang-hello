#!/usr/bin/env escript
%% -*- erlang -*-

main(_) ->
    S = io_lib:format("~p~n", [self()]),
    io:format(S).
