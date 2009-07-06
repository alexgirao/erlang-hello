-module(echo_server).
-author('Jesse E.I. Farmer <jesse@20bits.com>').

-export([loop/1]).

loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            gen_tcp:send(Socket, Data),
            loop(Socket);
        {error, closed} ->
            ok
    end.
