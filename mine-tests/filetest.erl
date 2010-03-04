-module(filetest).
-export([main/0]).

% erlc filetest.erl && erl -noshell -s filetest main -s init stop

main() ->
    T = erlang:decode_packet(fcgi, begin {ok, Data} = file:read_file("fcgireqtest.bin"), Data end, []),
    io:format("~p~n", [T]).
