-module(tut13).
-export([convert_list_to_c/1]).

%% tut13:convert_list_to_c([{moscow, {c, -10}}, {cape_town, {f, 70}}, {stockholm, {c, -4}}, {paris, {f, 28}}, {london, {f, 36}}]).

to_c({N, {f, T}}) ->
    {N, {c, trunc((T - 32) * 5 / 9)}};
to_c({N, {c, T}}) ->
    {N, {c, T}}.

convert_list_to_c(L) ->
    NL = lists:map(fun to_c/1, L),
    Test =
	fun ({_,{c,T1}}, {_,{c,T2}}) ->
		T1 < T2 end,
    lists:sort(Test, NL).
