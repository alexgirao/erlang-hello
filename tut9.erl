-module(tut9).
-export([test_if/2]).

%% tut9:test_if(5,33).
%% tut9:test_if(33,6).
%% tut9:test_if(2, 3).
%% tut9:test_if(1, 33).
%% tut9:test_if(33, 7).
%% tut9:test_if(33, 33).

test_if(A, B) ->
    if
	A == 5 ->
	    io:format("A == 5~n"),
	    a_equals_5;
	B == 6 ->
	    io:format("B == 6~n"),
	    b_equals_6;
	A == 2, B == 3 ->                        % A equals 2 and B equals 3
	    io:format("A == 2, B == 3~n"),
	    a_equals_2_and_b_equals_3;
	A == 1; B == 7 ->                        % A equals 1 or B equals 7
	    io:format("A == 1; B == 7~n"),
	    a_equals_1_or_b_equals_7
    end.
