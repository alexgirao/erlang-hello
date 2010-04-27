#!/usr/bin/env escript
%% -*- erlang -*-

main(_Args) ->
    {ok, FD} = file:open("sampledata.bin", [raw, write, delayed_write, binary]),
    file:write(FD, term_to_binary(1)),
    file:write(FD, term_to_binary(1.618034)),
    file:write(FD, term_to_binary(a_atom)),
    file:write(FD, term_to_binary("a_string")),
    file:write(FD, term_to_binary(true)),
    file:write(FD, term_to_binary(false)),
    file:write(FD, term_to_binary({a_atom, 1, 1.618034, "a_string"})),
    file:write(FD, term_to_binary([a_list, 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144])),
    ok = file:close(FD),
    io:format("ok~n").
