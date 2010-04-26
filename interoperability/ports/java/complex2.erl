-module(complex2).
-export([start/2, stop/0, init/2]).
-export([foo/1, bar/1]).

start(ExtPrg, Args) ->
    spawn(?MODULE, init, [ExtPrg, Args]).

stop() ->
    complex ! stop.

foo(X) ->
    call_port({foo, X}).

bar(Y) ->
    call_port({bar, Y}).

call_port(Msg) ->
    complex ! {call, self(), Msg},
    receive
        {complex, Result} ->
            Result
    end.

init(ExtPrg, Args) ->
    register(complex, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn_executable, ExtPrg}, [{packet, 4}, {args, Args}]),
    loop(Port).

loop(Port) ->
    receive
        {call, Caller, Msg} ->
            Port ! {self(), {command, encode(Msg)}},
            receive
                {Port, {data, Data}} ->
                    Caller ! {complex, decode(Data)}
            end,
            loop(Port);
        stop ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    exit(normal)
            end;
        {'EXIT', Port, _Reason} ->
            exit(port_terminated)
    end.

encode({foo, X}) -> [1, X];
encode({bar, Y}) -> [2, Y].

decode([Int]) -> Int.
