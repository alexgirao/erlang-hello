-module('flush-unknown').
-export([main/0, f1/0]).

f1() ->
    receive
	{hello, What} ->
	    io:format("hello ~p~n", [What]),
	    f1();
	{done, From} ->
	    From ! die;
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
	    MainPid ! finish;
	Msg ->
	    io:format("flushing unknown message from process ~p: ~p~n", [self(), Msg]),
	    f2(MainPid)
    end.

loop() ->
    receive
	finish ->
	    io:format("finishing main process ~p~n", [self()]);
	Msg ->
	    io:format("flushing unknown message from process ~p: ~p~n", [self(), Msg]),
	    loop()
    end.

main() ->
    Self = self(),

    PidF1 = spawn('flush-unknown', f1, []),             % spawn/3 (module, function, arguments)
    _PidF2 = spawn(fun() -> f2(Self, PidF1) end),       % spawn/1: (fun)

    loop().
