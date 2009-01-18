-module(tut8).
-export([reverse/1]).

% c(tut8).
% tut8:reverse([1,2,3]).

reverse(L) ->
    reverse(L, []).

reverse([H|T], R) ->
    reverse(T, [H|R]);
reverse([], R) ->
    R.
