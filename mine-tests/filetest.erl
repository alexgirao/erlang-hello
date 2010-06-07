-module(filetest).
-export([main/0]).

% erlc filetest.erl && erl -noshell -s filetest main -s init stop

main() ->
    T = erlang:decode_packet(fcgi, begin {ok, Data} = file:read_file("fcgireqtest.bin"), Data end, []),
    S = io_lib:format("~p~n", [T]),
    ok = file:write_file(atom_to_list(?MODULE) ++ ".out", S),
    io:format(S),
    ok.
