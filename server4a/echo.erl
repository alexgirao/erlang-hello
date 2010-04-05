-module(echo).

-export([init/1, do_echo/1]).

-define(TCP_OPTIONS, [
		      binary,
		      {packet, 0},
		      {active, false},
		      {reuseaddr, true}
		     ]
       ).

init(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    {ok, Ref} = prim_inet:async_accept(LSocket, -1),
    loop(LSocket, Ref).

loop(LSocket, Ref) ->
    ok.

%%

do_echo(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            gen_tcp:send(Socket, Data),
            do_echo(Socket);
        {error, closed} ->
            ok
    end.
