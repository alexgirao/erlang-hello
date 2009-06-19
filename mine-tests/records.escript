#!/usr/bin/env escript
%% -*- erlang, coding: utf-8 -*-

% all records are tagged by its name, but not by the tag structure

-record(person, {a, b}).

% is_record is a build-in-function (BIF) and available only at compile
% time, do not try to use it on erlang's shell

is_person(P) when is_record(P, person) -> true;
is_person(_) -> false.

main(_) ->
    make:files([records]),
    
    P0 = records:new_person("Alexandre Girão", 28),
    P1 = records:new_person("John Noage"),
    P2 = records:new_woman("Patricia Alencar", 26),
    T0 = records:new_test(),
    T1 = records:new_test(4),

    io:format("new person: ~p~n", [P0]),
    io:format("new person: ~p~n", [P1]),
    io:format("new woman: ~p~n", [P2]),
    io:format("test 0: ~p~n", [T0]),
    io:format("test 1: ~p~n", [T1])

    ,io:format("record field: ~p~n", [P0#person.a])      % accessing record members
    ,io:format("is a person?: ~p~n", [is_person(P0)])

    % pattern-matching on records

    ,S = "Alexandre Girão"
    ,R = case P0 of
            #person{a = S} -> yes;
            _ -> not_a_chance
        end
    
    ,io:format("is person ~p?: ~p~n", [S, R])

    .
