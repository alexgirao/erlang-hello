#!/usr/bin/env escript
%% -*- erlang -*-

main(_) ->
    make:files(['test-2']),
    'test-2':main().
