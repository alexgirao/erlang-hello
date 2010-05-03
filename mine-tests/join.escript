#!/usr/bin/env escript
%% -*- erlang -*-

main(_) ->
    make:files([string_join]),

    io:format("~p~n", [string_join:join(["1", "2", "3"], $,)]),
    % io:format("~p~n", [string_join:join([], "-")]),
    io:format("~p~n", [string_join:join(["alpha"], "-")]),
    io:format("~p~n", [string_join:join(["alpha", "bravo", "charlie"], $-)]),
    
    Items = ["bar foo" || _ <- lists:seq(1, 100000)],

    {Tmy_string_join, _} = timer:tc(string_join, my_string_join, [Items, $,]),
    {Tjoin, _} = timer:tc(string_join, join, [Items, $,]),

    io:format("Tmy_string_join: ~p~n", [Tmy_string_join]),
    io:format("Tjoin:           ~p~n", [Tjoin]),

    ok.
