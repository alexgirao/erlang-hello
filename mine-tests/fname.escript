#!/usr/bin/env escript
%% -*- erlang -*-

-define(debug_log_macros, true).
-include("$PWD/macro.hrl").

main(_Args) ->
    ?log_msg1("~p", [filename:join([os:getenv("HOME"), "a", "path", "to", "somewhere.ext"])]),

    gen_event:stop(error_logger), % this flushes error_logger messages from escript processes
    io:format("~n"),

    ok.
