-module(pingpong).
-export([start/0, ping/2, pong/0]).

%~ bash-3.2$ erlc pingpong.erl
%~ bash-3.2$ erl -noshell -s pingpong start -s init sto
%~ pong received ping 3
%~ ping received pong 3
%~ pong received ping 2
%~ ping received pong 2
%~ pong received ping 1
%~ ping received pong 1
%~ ping finished
%~ pong finished
%~ bash-3.2$

ping(0, Pong_PID) ->
    Pong_PID ! finished,
    io:format("ping finished~n");
ping(N, Pong_PID) ->
    Pong_PID ! {ping, self(), N},
    receive
	pong ->
	    io:format("ping received pong ~p~n", [N])
    end,
    ping(N - 1, Pong_PID).

pong() ->
    receive
	finished ->
	    io:format("pong finished~n");
	{ping, Ping_PID, Ping_count} ->
	    io:format("pong received ping ~p~n", [Ping_count]),
	    Ping_PID ! pong,
	    pong()
    end.

start() ->
    spawn(pingpong, ping, [3, spawn(pingpong, pong, [])]).
