#!/usr/bin/env escript
%% -*- erlang, coding: utf-8 -*-

-define(d, 1).
-define(D, "two").

main(_) ->
    io:format("~p ~p~n", [?d, ?D]).
