#!/usr/bin/env escript
%% -*- erlang -*-

-define(debug_log_macros, true).
-include("$PWD/log.hrl").

%?log_msg1("~p~n", [M]),

flush_unk() ->
    receive
	Msg ->
	    ?log_msg1("flushing unknown message from process ~p: ~p~n", [self(), Msg]),
	    flush_unk()
    after 0 ->
	    ok
    end.

main(_Args) ->
    true = code:add_patha("ebin"),
    ok = systools:make_tar("eb_rel-2"),

    %

    flush_unk(),
    gen_event:stop(error_logger), % this flushes error logger messages
    io:format("~nok~n"),
    ok.
