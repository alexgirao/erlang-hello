-module(tut6).
-export([list_max/1]).

% c(tut6).
% tut6:list_max([1,2,3,4,5,7,4,3,2,1]).

list_max([H|T]) ->                  % (H)ead and (T)ail
	list_max(T, H).

list_max([], R) ->                  % (R)esult
	R;
list_max([H|T], R) when R > H ->
	list_max(T, R);
list_max([H|T], R) ->
	list_max(T, H).
