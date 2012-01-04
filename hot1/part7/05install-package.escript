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
    ReleaseName = "eb_rel",
    ReleaseVsn = "2",
    Release = (ReleaseName ++ "-") ++ ReleaseVsn,
    Filename = Release ++ ".tar.gz",
    ReleaseDir = code:root_dir() ++ "/releases",
    
    S = Filename,
    D = ReleaseDir ++ "/" ++ Filename,

    ?log_msg1("copying ~p to ~p", [S, D]),

    {ok, _BytesCopied} = file:copy(S, D),
    ok = application:start(sasl),

    ?log_msg0("release_handler:unpack_release/1"),
    {ok, ReleaseVsn} = release_handler:unpack_release(Release),

    %

    % release_handler:install_release/1 seems unnecessary,
    % release_handler:unpack_release/1 already does this.. to be
    % confirmed

    ?log_msg0("release_handler:install_release/1"),

    M = release_handler:install_release("2"),
    ?log_msg1("~p~n", [M]),

    %

    flush_unk(),
    gen_event:stop(error_logger), % this flushes error logger messages
    io:format("~nok~n"),
    ok.
