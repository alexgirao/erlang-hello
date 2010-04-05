-module(echo1).

-export([init/1, do_echo/1, main/1]).

-define(TCP_OPTIONS, [
		      binary,
		      {packet, 0},
		      {active, false},
		      {reuseaddr, true}
		     ]
       ).

%% extracted from otp_src_R13B01-prim_inet.erl

%% setup options from listen socket on the connected socket
accept_opts(L, S) ->
    case prim_inet:getopts(L, [active, nodelay, keepalive, delay_send, priority, tos]) of
	{ok, Opts} ->
	    case prim_inet:setopts(S, Opts) of
		ok -> {ok, S};
		Error -> prim_inet:close(S), Error
	    end;
	Error ->
	    prim_inet:close(S), Error
    end.

%%

init(Port) when is_integer(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    {ok, Ref} = prim_inet:async_accept(LSocket, -1),   % prim_inet:async_accept(L, Time), -1=infinity
    loop(LSocket, Ref).

loop(LSocket, Ref) ->
    receive
	{inet_async, LSocket, Ref, {ok, S}} ->
	    % setup socket S

	    {ok, Mod} = inet_db:lookup_socket(LSocket),
	    io:format("socket accepted: ~p (module ~p)~n", [S, Mod]),
	    inet_db:register_socket(S, Mod),
	    accept_opts(LSocket, S),

	    % spawn worker and set the new controlling process

	    gen_tcp:controlling_process(S, spawn(fun() -> do_echo(S) end)),  

	    % new acceptor

	    {ok, NewRef} = prim_inet:async_accept(LSocket, -1),

	    % tail call

	    loop(LSocket, NewRef);

	{inet_async, _LSocket, Ref, _Error} = M ->
	    io:format("error: ~p~n", [M]);

	M ->
	    io:format("flushing unknown message ~p~n", [M]),
	    loop(LSocket, Ref)
    end.

%%

do_echo(Socket) ->
    process_flag(trap_exit, true),
    do_echo_loop(Socket).

do_echo_loop(Socket) ->
    prim_inet:async_recv(Socket, 0, -1),
    receive
	{inet_async, _LSocket, _Ref, {ok, Data}} ->
	    gen_tcp:send(Socket, Data),
	    do_echo_loop(Socket);

	{inet_async, _LSocket, _Ref, {error, _Error} = Err} = M ->
	    io:format("error: ~p~n", [M]),
	    Err;

	M ->
	    io:format("flushing unknown message ~p~n", [M]),
	    do_echo_loop(Socket)
    end.

%%

main([Port]) ->
    io:format("port used: ~p~n", [Port]),
    init(list_to_integer(atom_to_list(Port))).
