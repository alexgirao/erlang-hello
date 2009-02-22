-module(tut7).
-export([format_temps/1]).

% tut7:format_temps([{moscow, {c, -10}}, {cape_town, {f, 70}}, {stockholm, {c, -4}}, {paris, {f, 28}}, {london, {f, 36}}]).

format_temps(L) ->
    print_c(to_c(L)).

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
