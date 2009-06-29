-module(reverse_test).
-include_lib("eunit/include/eunit.hrl").

reverse_test() ->
	?assert(lists:reverse([1, 2, 3]) == [3, 2, 1]).
