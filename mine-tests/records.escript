#!/usr/bin/env escript
%% -*- erlang, coding: utf-8 -*-

% all records are tagged by its name, but not by the its structure

-record(person, {a, b}).

% is_record is a build-in-function (BIF) and available only at compile
% time, do not try to use it on erlang's shell

is_person(P) when is_record(P, person) -> true;
is_person(_) -> false.

get_name(P=#person{}) -> % pattern matching, P must be a 'person' record
    P#person.a.

get_name2(#person{a=Name}) -> % bind Name to P#person.a
    Name.

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

    ,io:format("get_name(P0): ~p~n", [get_name(P0)])
    ,io:format("get_name2(P0): ~p~n", [get_name2(P0)])

    % pattern-matching on records

    ,S = "Alexandre Girão"
    ,R = case P0 of
            #person{a = S} -> yes;
            _ -> not_a_chance
        end
    
    ,io:format("is person ~p?: ~p~n", [S, R])

    % record creation

    ,P3 = P0#person{b=29}
    ,io:format("~p age: ~p~n", [P3#person.a, P3#person.b])

    ,io:format("person_record_fields(): ~p~n", [records:person_record_fields()])

    ,io:format("P0: ~p~n", [records:debug_record(P0)])
    ,io:format("P1: ~p~n", [records:debug_record(P1)])
    ,io:format("P2: ~p~n", [records:debug_record(P2)])
    ,io:format("T0: ~p~n", [records:debug_record(T0)])
    ,io:format("T1: ~p~n", [records:debug_record(T1)])
    ,io:format("??: ~p~n", [records:debug_record({tag,1,2,3})])

    .
