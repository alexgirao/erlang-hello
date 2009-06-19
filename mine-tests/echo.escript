#!/usr/bin/env escript
%% -*- erlang -*-

main(_) ->
    make:files([echo]),
    echo:main().
