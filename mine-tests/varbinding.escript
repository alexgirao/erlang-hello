#!/usr/bin/env escript
%% -*- erlang, coding: utf-8 -*-

-record(r0, {f0, f1, f2=99}).  % r for record, f for field

t0({T0,_,_}) -> % bind on matching
    T0.

l0([L0,_,_]) -> % bind on matching
    L0.

r0f0(#r0{f0=R0}) -> % bind on matching, no need to specify all fields
    R0.

main(_) ->
    % variable binding on tuples

    T = {T0=1, T1=2, T2=3}, % bind on creation
    T = {1,2,3}, % verify
    T = {T0, T1, T2}, % verify

    {T10, T11, T12} = T, % bind on matching
    {T10, T11, T12} = {1,2,3}, % verify

    1 = t0(T),

    % variable binding on lists

    L = [L0=1, L1=2, L2=3], % bind on creation
    L = [1,2,3], % verify
    L = [L0, L1, L2], % verity

    [L10, L11, L12] = L, % bind on matching
    [L10, L11, L12] = [1,2,3], % verify

    1 = l0(L),

    % variable binding on records

    R = #r0{f0=R0=1, f1=R1=2, f2=R2=3}, % bind on creation
    R = #r0{f0=1, f1=2, f2=3}, % create without binding
    R = #r0{f0=R0, f1=R1, f2=R2}, % create from bound
    {1,2,3} = {R0, R1, R2}, % verify

    #r0{f0=R10, f1=R11, f2=R12} = R, % bind on matching
    {1,2,3} = {R10, R11, R12}, % verify

    #r0{f0=R20, f1=R21} = R, % bind on matching, no need to specify all fields
    {1,2} = {R20, R21}, % verify

    1 = r0f0(R),

    % 

    io:format("~p~n", [all_done]).
