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

unpack(ReleaseName, ReleaseVsn) ->
    Release = (ReleaseName ++ "-") ++ ReleaseVsn,
    Filename = Release ++ ".tar.gz",
    ReleaseDir = code:root_dir() ++ "/releases",
    
    S = Filename,
    D = ReleaseDir ++ "/" ++ Filename,

    ?log_msg1("copying ~p to ~p", [S, D]),

    {ok, _BytesCopied} = file:copy(S, D),
    {ok, ReleaseVsn} = release_handler:unpack_release(Release),
    ok.

upgrade({release, _, Vsn, EVsn, Apps, _} = _PermanentRelease) ->
    % create from.rel

    Rel0 = {release,
	    {"from", Vsn},
	    {erts, EVsn},
	    [{App, AppVsn} || {App, AppVsn, _AppPath} <- Apps]
	   },

    ok = file:write_file("from.rel", io_lib:format("~p.~n", [Rel0])),

    % create relup

    ok = systools:make_relup("eb_rel-1", ["from"], ["from"]),

    % remove "unpacked" state release from previous unpack, so we can
    % replace with new package below

    ok = release_handler:remove_release("1"),

    % create new eb_rel-1.tar.gz that includes relup

    ok = systools:make_tar("eb_rel-1"),

    % unpack release

    ReleaseName = "eb_rel",
    ReleaseVsn = "1",

    ok = unpack(ReleaseName, ReleaseVsn),

    ok.

main(_Args) ->
    true = code:add_patha("ebin"),

    % start sasl and wait just a second for error_logger message flush

    ok = application:start(sasl),
    0 = receive wont_match -> 1 after 1000 -> 0 end,

    % load ROOT/releases/RELEASES data

    {ok, [RELEASES]} = file:consult(code:root_dir() ++ "/releases/RELEASES"),

    % get permanent (official) release

    F = fun
	    ({release, _Desc, _Vsn, _EVsn, _Apps, permanent}) -> true;
	    (_I) -> false
	end,

    [{release, _, PermanentReleaseVsn, _, _, _} = PermanentRelease] = lists:filter(F, RELEASES),

    %

    % ?log_msg1("RELEASES=~p~n", [RELEASES]),
    % ?log_msg1("PermanentRelease=~p~n", [PermanentRelease]),

    % create from.rel

    case PermanentReleaseVsn of
	"1" ->
	    ?log_err1("release ~p is already in permanent state", [PermanentReleaseVsn]);
	_ ->
	    ok = upgrade(PermanentRelease)
    end,

    %

    flush_unk(),
    gen_event:stop(error_logger), % this flushes error logger messages
    io:format("~nok~n"),
    ok.
