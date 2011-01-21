-module(records).
-export([
    new_person/1, new_person/2,
    new_woman/2,
    new_test/0, new_test/1,
    person_record_fields/0,
    debug_record/1
    ]).

-record(person, {name, age}).

% record definitions are available at compile time only, runtime
% record definition work's as follows
%   rd(person, {name,age}).

-record(person_f, {name, age, sex=f}).   % sex has a default value
-record(test, {a, b, c, d, e=55, f, g, h}).

new_person(Name) ->
    #person{name=Name}.                  % will create a person with age=undefined

new_person(Name, Age) ->
    #person{name=Name, age=Age}.

new_woman(Name, Age) ->
    #person_f{name=Name, age=Age}.

% the special field _, means "all fields not explicitly specified".
% /doc/programming_examples/records.html#1.3

new_test() ->
    #test{a=1, b=2, _=3}.

new_test(V) ->
    #test{a=1, b=2, _=V}.

person_record_fields() ->
    record_info(fields, person).

zip(L, V) ->
    {element(1, V), zip(L, V, 2, [])}.
zip([H|T], V, I, Acc) ->
    zip(T, V, I+1, [{H, element(I, V)} | Acc]);
zip([], _, _, Acc) ->
    lists:reverse(Acc).

debug_record(R) ->
    A = case element(1, R) of
	person -> {ok, zip(record_info(fields, person), R)};
	person_f -> {ok, zip(record_info(fields, person_f), R)};
	test -> {ok, zip(record_info(fields, test), R)};
	_ -> {unknown, R}
    end.
