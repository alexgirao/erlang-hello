-module(echo4).

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
	    true = inet_db:register_socket(S, Mod),
	    accept_opts(LSocket, S),

	    % spawn worker and set the new controlling process

	    gen_tcp:controlling_process(S, spawn(fun() -> do_echo(S) end)),  

	    % new acceptor

	    {ok, NewRef} = prim_inet:async_accept(LSocket, -1),

	    % tail call

	    loop(LSocket, NewRef);

	{inet_async, LSocket, Ref, _Error} = M ->
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
    inet:setopts(Socket, [{active, once}]),
    receive
	{tcp, Socket, Data} = M ->
	    io:format("~p ~p~n", [Socket, M]),
	    try erlang:port_command(Socket, Data)
            catch error:Error -> exit(Error)
            end,
	    do_echo_loop(Socket);

	{tcp_closed, Socket} = M ->
	    io:format("~p ~p~n", [Socket, M]),
	    exit(normal);

	{tcp_error, Socket, Error} = M ->
	    io:format("~p ~p~n", [Socket, M]),
            exit(Error);

	% inet_reply comes from erlang:port_command/2

        {inet_reply, Socket, ok} = M ->
	    io:format("~p ~p~n", [Socket, M]),
            do_echo_loop(Socket);

        {inet_reply, Socket, Status} = M ->
	    io:format("~p ~p~n", [Socket, M]),
            exit(Status);

	%

	M ->
	    io:format("flushing unknown message ~p~n", [M]),
	    do_echo_loop(Socket)
    end.

%%

main([Port]) ->
    io:format("port used: ~p~n", [Port]),
    init(list_to_integer(atom_to_list(Port))).
