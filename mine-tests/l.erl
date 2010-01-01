-module(l).
-export([main/0]).

% using interactive shell
% $ erl
% 1> c(l).
% {ok,l}
% 2> l:main().
% [0,[1,2,3,4,5,6,7,8,9]]
% [0,[1,2,3,4,5,6,7,8,9]]
% ok
% 3>

% or using command line only
% erlc l.erl && erl -noshell -s l main -s init stop

main() ->
    L = lists:seq(0,9),
    io:format("~p~n~p~n", [
		       [hd(L), tl(L)], % hd and tl BIFs
		       case L of [H|T] -> [H,T] end % pattern matching
		      ]).
