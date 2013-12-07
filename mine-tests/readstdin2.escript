#!/usr/bin/env escript

% reference: http://stackoverflow.com/questions/10872909/erlang-read-stdin-write-stdout

doit() ->
  A = io:get_line(standard_io, "line?> "),
  case A of
    E = {error, _} ->
      throw(E);
    eof -> ok;
    _ ->
      io:format("got ~p~n", [A]),
      doit()
  end.

main(_) ->
  doit(),
  ok.
