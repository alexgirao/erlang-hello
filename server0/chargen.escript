#!/usr/bin/env escript
%% -*- erlang -*-

main(_) ->
    make:files([chargen]),
    chargen:listen(1234).
