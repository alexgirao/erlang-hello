%% 'tut7-2':format_temps([{moscow, {c, -10}}, {cape_town, {f, 70}}, {stockholm, {c, -4}}, {paris, {f, 28}}, {london, {f, 36}}]).

-module('tut7-2').
-export([format_temps/1]).

format_temps(L) ->
    Lc = to_c(L),
    print_c(Lc),
    {Max, Min} = maxmin(Lc),
    print_maxmin(Max, Min).

to_c([{N, {f, F}} | T]) ->
    [{N, {c, (F - 32) * 5 / 9}} | to_c(T)];
to_c([H | T]) ->     % no conversion needed
    [H | to_c(T)];
to_c([]) ->
    [].

print_c([H|T]) ->
    {N, {c, C}} = H,
    io:format("~-15w ~w c~n", [N, C]),
    print_c(T);
print_c([]) ->
    ok.

maxmin([H|T]) ->
    maxmin(T, H, H).

maxmin([H|T], Max, Min) ->
    {_, {c, TeH}} = H,                       % _ means "don't care"
    {_, {c, TeMax}} = Max,
    {_, {c, TeMin}} = Min,
    if
	TeH > TeMax ->
	    MaxR = H;                        % the head is the new max
	true ->
	    MaxR = Max
    end,
    if
	TeH < TeMin ->
	    MinR = H;                        % the head is the new min
	true ->
	    MinR = Min
    end,
    maxmin(T, MaxR, MinR);
maxmin([], Max, Min) ->                      % processed all items
    {Max, Min}.

print_maxmin({MaxN, {c, TeMax}}, {MinN, {c, TeMin}}) ->
    io:format("max temperature was ~w c in ~w~n", [TeMax, MaxN]),
    io:format("min temperature was ~w c in ~w~n", [TeMin, MinN]).
