-module('test-0').
-export([main/0, startup/2, shutdown/2, connection_made/1, echo/1]).

% make && erl -noshell -s test-0 main -s init stop
% for i in `seq 1 3000`; do date '+%N' | nc -q1 localhost 8070; done

-define(TCP_OPTIONS, [
		      binary,
		      {packet, 0},
		      {active, false},
		      {reuseaddr, true}
		     ]
       ).

startup(_IPAddress, _Port) ->
    io:format("startup~n"),
    ok.

shutdown(_IPAddress, _Port) ->
    io:format("shutdown~n"),
    ok.

connection_made(Socket) ->
    % 
    WorkerPID = proc_lib:spawn_link(?MODULE, echo, [Socket]),
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
	    {?MODULE, connection_made, []},
	    ?MODULE % label
	   ),
    receive
	Msg ->
	    io:format("main loop message: ~p~n", [Msg])
    end,
    ok.
