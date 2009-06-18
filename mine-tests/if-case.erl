-module('if-case').
-export([month_length/2]).

%% 'if-case':month_length(2004, feb).
%% 'if-case':month_length(2003, feb).
%% 'if-case':month_length(1947, aug).

month_length(Y, M) ->
	%% All years divisible by 400 are leap
	%% Years divisible by 100 are not leap (except the 400 rule above)
	%% Years divisible by 4 are leap (except the 100 rule above)
	IsLeap =
	if
		trunc(Y / 400) * 400 == Y ->
			true;
		trunc(Y / 100) * 100 == Y ->
			false;
		trunc(Y / 4) * 4 == Y ->
			true;
		true ->         % always match, set last
			false
	end,
	case M of
		sep -> 30;
		apr -> 30;
		jun -> 30;
		nov -> 30;
		feb when IsLeap == true -> 29;
		feb -> 28;
		jan -> 31;
		mar -> 31;
		may -> 31;
		jul -> 31;
		aug -> 31;
		oct -> 31;
		dec -> 31
	end.
