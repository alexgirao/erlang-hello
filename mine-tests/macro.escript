#!/usr/bin/env escript
%% -*- erlang -*-

-define(debug_log_macros, true).
-include("$PWD/macro.hrl").

main(_Args) ->
    Source = "macro.erl",
    {ok, []} = compile:file(Source, ['P', {d, debug_log_macros}]), % preprocessor, generate macro.P
    {ok, macro} = compile:file(Source, ['E', {d, debug_log_macros}]), % preprocessor and transformations, generate macro.E
    {ok, macro} = compile:file(Source, [{d, debug_log_macros}]), % compile, generate macro.beam
    macro:main(), % run

    ?log_msg0("alpha"),
    ?log_msg1("~p~n", [bravo]),
    ?log_msg1("~p/~p~n", [charlie, delta]),

    gen_event:stop(error_logger), % this flushes error_logger messages from escript processes
    io:format("~n"),

    ok.
