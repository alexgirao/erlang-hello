#!/usr/bin/env escript
%% -*- erlang -*-

main(_Args) ->
    FN = "t2b.escript.bin",
    {ok, FD} = file:open(FN, [raw, write, delayed_write, binary]),
    L = [
	 1,
	 1.618034,
	 a_atom,
	 "a_string",
	 true,
	 false,
	 {a_atom, 1, 1.618034, "a_string"},
	 [a_list, 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144]
	],
    file:write(FD, term_to_binary(L, [{minor_version, 1}])),     % {minor_version, 1} force use of IEEE 754 floating-point "double format" bit layout
    %file:write(FD, term_to_binary(L, [{minor_version, 0}])),   % erl_interface (c library) does not support IEEE 754 serialization
    ok = file:close(FD),
    io:format("wrote ~p~n", [FN]).
