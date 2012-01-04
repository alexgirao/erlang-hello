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

get_app_key(App, Key) ->
    {ok, D} = application:get_all_key(App),
    proplists:get_value(Key, D).

main(_Args) ->
    ok = application:start(sasl),

    ok = application:load(erlybank),

    AppVsn = get_app_key(erlybank, vsn),

    ?log_msg1("installed version AppVsn=~p", [AppVsn]),
    ?log_msg1("releases=~p", [release_handler:which_releases()]),

    M = release_handler:make_permanent("2"),
    ?log_msg1("~p~n", [M]),
    ?log_msg1("releases=~p", [release_handler:which_releases()]),

    %

    flush_unk(),
    gen_event:stop(error_logger), % this flushes error logger messages
    io:format("~nok~n"),
    ok.
