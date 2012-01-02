-module('test-1').
-export([main/0, startup/2, shutdown/2, connection_made/1, echo/1]).

% make && erl -noshell -s test-1 main -s init stop
% for i in `seq 1 3000`; do date '+%N' | nc -q1 localhost 8070; done

-define(TCP_OPTIONS, [
		      binary,
		      {packet, 0},
		      {active, false},
		      {reuseaddr, true}
		     ]
       ).

startup(IPAddress, Port) ->
    io:format("~p:startup(~p, ~p) called~n", [?MODULE, IPAddress, Port]),
    ok.

shutdown(IPAddress, Port) ->
    io:format("~p:shutdown(~p, ~p) called~n", [?MODULE, IPAddress, Port]),
    ok.

connection_made(Socket) ->
    WorkerPID = proc_lib:spawn_link(?MODULE, echo, [Socket]),
    io:format("~p:connection_made(~p) called on acceptor ~p, new worker ~p created~n", [?MODULE, Socket, self(), WorkerPID]),
    gen_tcp:controlling_process(Socket, WorkerPID),
    WorkerPID.

echo(Socket) ->
    process_flag(trap_exit, true),
    echo_loop(Socket).

echo_loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            gen_tcp:send(Socket, Data),
            echo_loop(Socket);
        {error, closed} ->
	    io:format("we're done with worker ~p~n", [self()]),
	    exit(normal);
        R ->
	    io:format("unknown result, exiting: ~p~n", [R]),
	    exit(normal)
    end.

main() ->
    file_handle_cache:start_link(),
    tcp_listener_sup:start_link(
	    {127, 0, 0, 1},
	    8070,
	    ?TCP_OPTIONS,
	    {?MODULE, startup, []},
	    {?MODULE, shutdown, []},
	    {?MODULE, connection_made, []},  % AcceptCallback
	    5,  % ConcurrentAcceptorCount
	    ?MODULE % label
	   ),
    receive
	Msg ->
	    io:format("main loop message: ~p~n", [Msg])
    end,
    ok.
