#!/usr/bin/env escript
%% -*- erlang -*-

%% man escript
%% ...
%% Pre-defined macros (such as ?MODULE) will not work. A script does not
%% have module name, so BIFs such as spawn/3 that require a module name
%% cannot be used. Instead, use a BIF that take a fun, such as spawn/1.
%% ...

main(_) ->
    make:files(['flush-unknown']),
    'flush-unknown':main().
