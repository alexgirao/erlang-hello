#!/usr/bin/env escript
%% -*- erlang -*-

main(_) ->
    D0 = dict:new(),
    D1 = dict:store(a, b, D0),
    D2 = dict:store(a, c, D1),

    {'EXIT', {badarg, [{dict, fetch, [a, D0]} | _]}} = (catch dict:fetch(a, D0)),
    b = dict:fetch(a, D1),
    c = dict:fetch(a, D2),

    false = dict:is_key(a, D0),
    true = dict:is_key(a, D1),
    true = dict:is_key(a, D2),

    error = dict:find(a, D0),
    {ok, b} = dict:find(a, D1),
    {ok, c} = dict:find(a, D2),

    D3 = dict:update(a, fun (X) -> atom_to_list(X) end, D2),
    "c" = dict:fetch(a, D3),

    D4 = dict:erase(a, D3),

    {'EXIT', {badarg, [{dict, fetch, [a, D4]} | _]}} = (catch dict:fetch(a, D4)),
    {'EXIT', {badarg, [{dict, update, [a, _, D4]} | _]}} = (catch dict:update(a, fun (X) -> atom_to_list(X) end, D4)),

    ok.
