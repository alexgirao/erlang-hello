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
    true = code:add_patha("ebin"),  % prepend 'ebin' dir, so we load this application before the one already deployed
    ok = systools:make_relup("eb_rel-2", ["eb_rel-1"], ["eb_rel-1"]),

    ok = systools:make_script("eb_rel-2", [local]),

    %

    flush_unk(),
    gen_event:stop(error_logger), % this flushes error logger messages
    io:format("~nok~n"),
    ok.
