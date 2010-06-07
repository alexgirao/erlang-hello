#!/usr/bin/env escript
%% -*- erlang -*-

main(_Args) ->
    L = [
	 1,
	 1.618034,
	 a_atom,
	 "a string",
	 true,
	 false,
	 {a_tuple, 1, 1.618034, "another string"},
	 [a_improper_list, [c, d, [e | f]]],
	 [a_list, 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]
	],

    FN1 = "t2b.escript.1.bin",
    ok = file:write_file(FN1, term_to_binary(L, [{minor_version, 1}])),
    io:format("wrote ~p~n", [FN1]),
    
    FN0 = "t2b.escript.0.bin",
    ok = file:write_file(FN0, term_to_binary(L)),
    io:format("wrote ~p~n", [FN0]),

    ok.
