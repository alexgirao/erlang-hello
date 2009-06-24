#!/usr/bin/env escript
%% -*- erlang -*-

main(_) ->
    make:files([tt]),
    R = timer:tc(erlang, length, [[]]),
    io:format("~p~n", [
        R
    ]).
