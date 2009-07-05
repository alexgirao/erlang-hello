-module(p).
-export([main/0]).

% erlc p.erl && erl -noshell -s p main -s init stop

pong() ->
    io:format("process pong (~p) started~n", [self()]),
    pong(0).

pong(N) ->
    receive
	ping ->
	    io:format("pong ~p~n", [N]),
	    pong(N+1);
	{timeout, _T, let_trap} ->
	    process_flag(trap_exit, true),    % let pong make a choice about his survival
	    pong(N);
	{'EXIT', _From, Reason} ->
	    io:format("pong: trap-exit with ~p~n", [Reason]),
	    pong(N);
        M ->
	    io:format("pong: unknown message at ~p: ~p~n", [self(), M]),
	    pong(N)
    end.

lp([]) ->
    ok;
lp([Pid|Others]) ->
    io:format("~p = ~p (or ~p)~n",
	      [
	       Pid,
	       lists:keysearch(registered_name, 1, process_info(Pid)),
	       process_info(Pid, registered_name)
	      ]),
    lp(Others).

loop(Pong) ->
    Pong_status =
	case process_info(Pong, status) of
	    {status, S} ->
		S;
	    undefined ->
		dead
	end,
    io:format("pong status: ~p~n", [Pong_status]),
    receive
	{timeout, _T, ping} ->
	    Pong ! ping,
	    loop(Pong);
	{timeout, _T, {exit, kill}} ->
	    io:format("ping: exterminating pong (wont trap-exit)~n"),
	    exit(Pong, kill),
	    loop(Pong);
	{timeout, _T, {exit, Reason}} ->
	    exit(Pong, Reason),
	    loop(Pong);
	{timeout, _T, done} ->
	    ok;
	Any ->
	    io:format("ping: unknown message at ~p: ~p~n", [self(), Any]),
	    loop(Pong)
    end.

main() ->
    lp(lists:sublist(processes(), 1, 10)),
    io:format("~p~n", [process_info(whereis(hd(registered())))]),
    
    Pong = spawn(fun pong/0),

    % serializing with a timer, because exit/2 acts immediately as
    % oposed to the process message box

    erlang:start_timer(10, self(), ping), % pong 0
    erlang:start_timer(20, self(), {exit, normal}),
    erlang:start_timer(30, self(), ping), % pong 1
    erlang:start_timer(40, Pong, let_trap),
    erlang:start_timer(50, self(), {exit, could_you_die_please}),
    erlang:start_timer(60, self(), ping), % pong 2
    erlang:start_timer(70, self(), {exit, kill}),
    erlang:start_timer(80, self(), ping), % no pong
    erlang:start_timer(90, self(), done),

    loop(Pong).
