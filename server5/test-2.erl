-module('test-2').
-export([main/0, startup/2, shutdown/2, connection_made/1, echo/1]).

% ERL_LIBS="$(pwd)" erl -boot start_sasl
% 1> make:files([file_handle_cache, rabbit_misc, tcp_acceptor, tcp_acceptor_sup, tcp_listener, tcp_listener_sup, 'test-0', 'test-1', 'test-2']).
% 2> spawn('test-2', main, []).
%
% erl -noshell -s test-2 main -s init stop
% 
% perl -we"print(qq{\x00\x00\x00\x03a\r\n\x00\x00\x00\x04ab\r\n});" | nc 192.168.137.1 8070 | od -t x1 -c
%

-define(TCP_OPTIONS, [
		      binary,
		      {packet, 4},
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
	    {0, 0, 0, 0},   % {127, 0, 0, 1},
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
