-module(tut15).
-export([start/0, ping/2, pong/0]).

ping(0, Pong_PID) ->
    Pong_PID ! finished,
    io:format("ping finished~n");
ping(N, Pong_PID) ->
    Pong_PID ! {ping, self()},
    receive
	pong ->
	    io:format("ping received pong~n")
    end,
    ping(N - 1, Pong_PID).

pong() ->
    receive
	finished ->
	    io:format("pong finished~n");
	{ping, Ping_PID} ->
	    io:format("pong received ping~n"),
	    Ping_PID ! pong,
	    pong()
    end.

start() ->
%%     Pong_PID = spawn(tut15, pong, []),
%%     spawn(tut15, ping, [3, Pong_PID]).
    spawn(tut15, ping, [3, spawn(tut15, pong, [])]).
