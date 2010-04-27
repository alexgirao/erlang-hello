#!/usr/bin/env escript
%% -*- erlang -*-

main(_Args) ->
    {ok, FD} = file:open("sampledata2.bin", [raw, write, delayed_write, binary]),
    file:write(FD, term_to_binary(
		     [
		      1,
		      1.618034,
		      a_atom,
		      "a_string",
		      true,
		      false,
		      {a_atom, 1, 1.618034, "a_string"},
		      [a_list, 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144]
		     ]
		    )),
    ok = file:close(FD),
    io:format("ok~n").
