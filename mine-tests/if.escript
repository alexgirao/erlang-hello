#!/usr/bin/env escript
%% -*- erlang, coding: utf-8 -*-

pl([H|T]) ->
    io:format("~p~n", [H]),
    pl(T);
pl([]) ->
    ok.

main(_) ->
    make:files(['if']),
    R = [
	'if':test_if(5,33),
	'if':test_if(33,6),
	'if':test_if(2, 3),
	'if':test_if(20, 30),
	'if':test_if(1, 33),
	'if':test_if(33, 7),
	'if':test_if(10, 33),
	'if':test_if(33, 70),
	'if':test_if(33, 33)
    ],
    pl(R).
