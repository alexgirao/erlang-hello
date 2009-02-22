-module(tut14).
-export([start/0, say_something/2]).

%% tut14:say_something(hello, 3).
%% tut14:start().

say_something(What, 0) ->
    done;
say_something(What, Times) ->
    io:format("~p~n", [What]),
    say_something(What, Times - 1).

start() ->
    spawn(tut14, say_something, [hello, 3]),
    spawn(tut14, say_something, [goodbye, 3]).
