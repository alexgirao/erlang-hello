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

% debug_record

zip([H|T], V) ->
    zip(T, V, 3, [{H, element(2, V)}]).
zip([H|T], V, I, Acc) ->
    zip(T, V, I+1, [{H, element(I, V)} | Acc]);
zip([], V, _, Acc) ->
    {element(1, V), lists:reverse(Acc)}.

-define(r(I), I -> {ok, zip(record_info(fields, I), R)}).

debug_record(R) when is_tuple(R) ->
    case element(1, R) of
	?r(person);
	?r(person_f);
	?r(test);
	_ -> {unknown, R}
    end.
