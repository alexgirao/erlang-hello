#!/usr/bin/env escript
%% -*- erlang -*-

-define(debug_log_macros, true).
-include("$PWD/macro.hrl").

main(_Args) ->

    ?log_msg1("first 5 path=~p", [lists:sublist(code:get_path(), 5)]),

    code:add_patha("../hot0"),
    code:add_pathz("../hot1"),

    % first 3

    ?log_msg1("first 3 path=~p", [lists:sublist(code:get_path(), 3)]),

    % last 3

    P = code:get_path(),
    ?log_msg1("last 3 path=~p", [lists:nthtail(length(P) - 3, P)]),

    %

    gen_event:stop(error_logger), % this flushes error_logger messages from escript processes
    io:format("~n"),

    ok.
