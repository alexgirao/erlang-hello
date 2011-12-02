#!/usr/bin/env escript
%% -*- erlang -*-

-define(debug_log_macros, true).
-include("$PWD/macro.hrl").

main(_Args) ->
    {ok, A} = file:consult("config-1.cfg"),
    {ok, B} = file:consult("config-2.cfg"),

    ?log_msg1("~n~p~n~p~n", [A, B]),

    %%%%%%%%%%

    gen_event:stop(error_logger), % this flushes error_logger messages from escript processes
    io:format("~n"),
    ok.
