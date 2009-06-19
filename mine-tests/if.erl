-module('if').
-export([test_if/2]).

%% 'if':test_if(5,33).
%% 'if':test_if(33,6).
%% 'if':test_if(2, 3).
%% 'if':test_if(1, 33).
%% 'if':test_if(33, 7).
%% 'if':test_if(20, 30).
%% 'if':test_if(10, 33).
%% 'if':test_if(33, 70).
%% 'if':test_if(33, 33).

test_if(A, B) ->
	if
        A == 5 ->
            a_equals_5;
        B == 6 ->
            b_equals_6;
        A == 2, B == 3 ->                   % A equals 2 and B equals 3
            a_equals_2_and_b_equals_3;
        A == 20 andalso B == 30 ->          % A equals 20 and B equals 30, andalso uses short-circuit
            a_equals_20_and_b_equals_30;
        A == 1; B == 7 ->                   % A equals 1 or B equals 7
            a_equals_1_or_b_equals_7;
        A == 10 orelse B == 70 ->           % A equals 10 or B equals 70, orelse uses short-circuit
            a_equals_10_or_b_equals_70;
	true ->
		none_before
	end.
