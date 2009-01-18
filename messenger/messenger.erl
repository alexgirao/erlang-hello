-module(messenger).
-export([
	 start_server/0,
	 server/1,
	 logon/1,
	 logoff/0,
	 message/2,
	 client/2
	]).

server_node() ->
    alpha@giga.

server(L) ->
    receive
	%% F is from, N is name, T is to
	{F, logon, N} ->
	    server(server_logon(F, N, L));
	{F, logoff} ->
	    server(server_logoff(F, L));
	{F, message_to, To, Msg} ->
	    server_transfer(F, To, Msg, L),
	    io:format("list is now: ~p~n", [L]),
	    server(L)
    end.

start_server() ->
    register(messenger, spawn(messenger, server, [[]])).

server_logon(F, N, L) ->
    case lists:keymember(N, 2, L) of
	true ->
	    F ! {messenger, stop, user_exists_at_other_node},
	    L;
	false ->
	    F ! {messenger, logged_on},
	    [{F, N} | L]
    end.

server_logoff(F, L) ->
    lists:keydelete(F, 1, L).

server_transfer(F, To, Msg, L) ->
    case lists:keysearch(F, 1, L) of         % search sender name by node
	false ->
	    F ! {messenger, stop, you_are_not_logged_on};
	{value, {F, From}} ->
	    server_transfer(F, From, To, Msg, L)
    end.

server_transfer(F, From, To, Msg, L) ->
    case lists:keysearch(To, 2, L) of         % search recipient node by name
	false ->
	    F ! {messenger, receiver_not_found};
	{value, {T, _}} ->
	    T ! {message_from, From, Msg},
	    F ! {messenger, sent}
    end.

logon(N) ->
    case whereis(mess_client) of
	undefined ->
	    register(mess_client,
		     spawn(messenger, client, [server_node(), N]));
	_ ->
	    already_logged_on
    end.

logoff() ->
    mess_client ! logoff.

message(To, Msg) ->
    case whereis(mess_client) of
	undefined ->
	    not_logged_on;
	_ ->
	    mess_client ! {message_to, To, Msg},
	    ok
    end.

client(S, N) ->
    {messenger, S} ! {self(), logon, N},
    await_result(),
    client(S).

client(S) ->
    receive
	logoff ->
	    {messenger, S} ! {self(), logoff},
	    exit(normal);
	{message_to, To, Msg} ->
	    {messenger, S} ! {self(), message_to, To, Msg},
	    await_result();
	{message_from, From, Msg} ->
	    io:format("msg from ~p: ~p~n", [From, Msg])
    end,
    client(S).

await_result() ->
    receive
	{messenger, stop, Why} ->
	    io:format("stop: ~p~n", [Why]),
	    exit(normal);
	{messenger, What} ->
	    io:format("~p~n", [What])
    end.
