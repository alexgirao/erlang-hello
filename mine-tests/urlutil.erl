-module(urlutil).
-export([test_avg/4]).
-export([parse_scheme/1, parse_scheme2/1, parse_scheme3/1]).

% reference: http://userprimary.net/user/2009/02/09/learning-string-manipulation-in-erlang/

% urlutil:test_avg(urlutil, parse_scheme, ["http://foo"], 10000).
% urlutil:test_avg(urlutil, parse_scheme2, ["http://foo"], 10000).
% urlutil:test_avg(urlutil, parse_scheme3, ["http://foo"], 10000).

%

parse_scheme(Url) ->
    Pos = string:str(Url, "://"),
    if
       Pos > 1, length(Url) > (Pos + 3) ->
            {string:substr(Url, 1, Pos - 1), string:substr(Url, Pos + 3)};
       true ->
            {bad_scheme, Url}
    end.

%

parse_scheme2(Url) ->
    parse_scheme2(Url, []).

parse_scheme2([], Acc) ->
    {bad_scheme, lists:reverse(Acc)};
parse_scheme2("://" ++ Rest, Acc) when length(Rest) > 0 ->
    {lists:reverse(Acc), Rest};
parse_scheme2([C|Rest], Acc) ->
    parse_scheme2(Rest, [C|Acc]).

%

parse_scheme3(Url) ->
    parse_scheme3(Url, []).

parse_scheme3([], Acc) ->
    {bad_scheme, lists:reverse(Acc)};
parse_scheme3([$:, $/, $/, C | Rest], Acc) ->
    {lists:reverse(Acc), [C|Rest]};
parse_scheme3([C|Rest], Acc) ->
    parse_scheme2(Rest, [C|Acc]).

%

%% reference: http://www.trapexit.org/Measuring_Function_Execution_Time

test_avg(M, F, A, N) when N > 0 ->
    L = test_loop(M, F, A, N, []),
    Length = length(L),
    Min = lists:min(L),
    Max = lists:max(L),
    Med = lists:nth(round((Length / 2)), lists:sort(L)),
    Avg = round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length),
    io:format("Range: ~b - ~b mics~n"
	      "Median: ~b mics~n"
	      "Average: ~b mics~n",
	      [Min, Max, Med, Avg]),
    Med.

test_loop(_M, _F, _A, 0, List) ->
    List;

test_loop(M, F, A, N, List) ->
    {T, _Result} = timer:tc(M, F, A),
    test_loop(M, F, A, N - 1, [T|List]).
