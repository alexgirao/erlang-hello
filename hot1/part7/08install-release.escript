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
    % start sasl and wait just a second for error_logger message flush

    ok = application:start(sasl),
    0 = receive wont_match -> 1 after 1000 -> 0 end,

    %
    
    [{_, Vsn, _Apps, permanent}] = release_handler:which_releases(permanent),

    % release_handler:install_release/1 has no permanent effects

    case Vsn of
	"2" ->
	    ?log_err1("release ~p is already in permanent state", [Vsn]);
	_ ->
	    {ok, _OtherVsn, _Descr} = release_handler:install_release("2")
    end,

    %

    flush_unk(),
    gen_event:stop(error_logger), % this flushes error logger messages
    io:format("~nok~n"),
    ok.
