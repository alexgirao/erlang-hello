#!/usr/bin/env escript

% reference: http://stackoverflow.com/questions/10872909/erlang-read-stdin-write-stdout

main(_) ->
  {ok, [X]} = io:fread("How many Hellos?> ", "~d"),
  hello(X).

hello(N) when N =< 0 ->
  ok;
hello(N) ->
  io:fwrite("Hello World!~n"),
  hello(N - 1).
