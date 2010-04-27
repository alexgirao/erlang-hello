#!/usr/bin/env escript
%% -*- erlang -*-

readterms() ->
    {ok, Data} = file:read_file("writeterms.bin"),
    Data.

main(_Args) ->
    io:format("~p~n", [readterms()]).
