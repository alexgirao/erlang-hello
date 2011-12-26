#!/usr/bin/env escript
%% -*- erlang -*-

-define(debug_log_macros, true).
-include("$PWD/macro.hrl").

flush_unk() ->
    receive
	Msg ->
	    ?log_msg1("flushing unknown message from process ~p: ~p~n", [self(), Msg]),
	    flush_unk()
    after 0 ->
	    ok
    end.

main(_Args) ->
    self() ! msg_1,
    self() ! msg_2,

    0 = receive wont_match -> 1 after 1000 -> 0 end,

    self() ! msg_3,
    self() ! msg_4,

    flush_unk(),

    gen_event:stop(error_logger), % this flushes error_logger messages from escript processes
    io:format("~n"),

    ok.
