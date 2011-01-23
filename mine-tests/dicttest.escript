#!/usr/bin/env escript
%% -*- erlang -*-

main(_) ->
    D0 = dict:new(),

    % store = insert or update

    D1 = dict:store(a, first_value, D0), % insert
    D2 = dict:store(a, [1], D1), % update
    D3 = dict:store(z, z_value, D2), % insert, used for erase example below

    % append/3 = append a value to the current list of values or error
    % append_list/3 = append a list of values to the current list of values or error
    % - didn't find "insert or error" on api, append is the one the
    %   comes closest, but require value to be a list

    D4 = dict:append(a, 2, D3),
    D5 = dict:append_list(a, [3, 4, 5], D4),

    % update/3 = update or error
    % update/4 = update or set initial value

    D6 = dict:update(a, fun (X) -> list_to_tuple(X) end, D5),  % now 'a' is a tuple instead of a list
    D7 = dict:update(b, fun (X) -> list_to_tuple(X) end, [], D6),  % fun won't be applied to initial value

    % erase/2 = erase or no action

    D8 = dict:erase(z, D7),
    D = dict:erase(unknown, D8),

    % fetch = fetch or error
    % 

    {1, 2, 3, 4, 5} = dict:fetch(a, D),
    [] = dict:fetch(b, D),
    {'EXIT', {badarg, _}} = (catch dict:fetch(unknown, D)),

    % find(Key, Dict) -> {ok, Value} | error

    error = dict:find(unknown, D),
    {ok, {1, 2, 3, 4, 5}} = dict:find(a, D),
    {ok, []} = dict:find(b, D),

    % is_key(Key, Dict) -> bool()

    true = dict:is_key(a, D),
    false = dict:is_key(z, D),

    %

    L = dict:to_list(D),
    {a, {1, 2, 3, 4, 5}} = lists:keyfind(a, 1, L),
    {b, []} = lists:keyfind(b, 1, L),

    E = dict:from_list(L),
    {ok, {1, 2, 3, 4, 5}} = dict:find(a, E),

    ok.
