-module('flush-unknown').
-export([main/0, f1/0]).

% note: it's highly recommended that looping functions that receive
% messages be tail recursive, as f1/0, f2/1 and loop/0 are

f1() ->
    receive
	{hello, What} ->
	    io:format("hello ~p~n", [What]),
	    f1();
	{done, From} ->
	    From ! die;  % won't recurse, exit
	Msg ->
	    io:format("flushing unknown message from process ~p: ~p~n", [self(), Msg]),
	    f1()
    end.

f2(MainPid, F1) ->
    F1 ! {hello, "world!"},        % list
    F1 ! {hello, 'world!'},        % atom
    F1 ! discard_me,
    F1 ! {discard_me, too},
    F1 ! "discard_me_too",
    F1 ! {done, self()},
    f2(MainPid).

f2(MainPid) ->
    receive
	die ->
	    MainPid ! finish;  % won't recurse, exit
	Msg ->
	    io:format("flushing unknown message from process ~p: ~p~n", [self(), Msg]),
	    f2(MainPid)
    end.

loop() ->
    receive
	finish ->
	    io:format("finishing main process ~p~n", [self()]);  % won't recurse, exit
	Msg ->
	    io:format("flushing unknown message from process ~p: ~p~n", [self(), Msg]),
	    loop()
    end.

main() ->
    Self = self(),

    PidF1 = spawn(?MODULE, f1, []),                     % spawn/3 (module, function, arguments)
    _PidF2 = spawn(fun() -> f2(Self, PidF1) end),       % spawn/1: (fun)

    loop().
