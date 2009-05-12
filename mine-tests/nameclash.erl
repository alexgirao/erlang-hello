#!/usr/bin/env escript
%% -*- erlang -*-

io() ->
    "no conflict will happen".

main(_Args) ->
    io:fwrite("hello world! ~p~n", [io()]).
