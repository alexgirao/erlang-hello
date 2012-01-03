#!/usr/bin/env escript
%% -*- erlang -*-

-define(debug_log_macros, true).
-include("$PWD/macro.hrl").

main(_Args) ->
    ?log_msg1("erts version=~p", [erlang:system_info(version)]),

    gen_event:stop(error_logger), % this flushes error_logger messages from escript processes
    io:format("~n"),

    ok.
