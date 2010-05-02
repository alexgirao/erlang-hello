#!/usr/bin/env escript
%% -*- erlang -*-

% full list at erts/emulator/beam/external.h

-define(ATOM_EXT,      100).
-define(PID_EXT,       103).
-define(NIL_EXT,       106).   % also represented by an empty list []
-define(LIST_EXT,      108).

-define(VERSION_MAGIC, 131).   % 131 since erts-4.3

% list serialization

% strip VERSION_MAGIC
t2b(T) ->
    {<<?VERSION_MAGIC>>, R} = split_binary(term_to_binary(T), 1),
    R.

make_list(A, B, C) ->
    iolist_to_binary(
      [
       <<?VERSION_MAGIC>>,
       <<?LIST_EXT,0,0,0,3>>,    % list header with 3 items (32-bit big-endian)
       t2b(A),
       t2b(B),
       t2b(C),
       <<?NIL_EXT>>   % every list must be followed by another term, NIL if no more items
      ]).

make_proper_list(A, B, C) ->
    iolist_to_binary(
      [
       <<?VERSION_MAGIC>>,
       <<?LIST_EXT,0,0,0,1>>,
       t2b(A),
       <<?LIST_EXT,0,0,0,1>>,
       t2b(B),
       <<?LIST_EXT,0,0,0,1>>,
       t2b(C),
       <<?NIL_EXT>>
      ]).

main(_) ->
    io:format("list:~n ~p ->~n ~p ->~n ~p.~n",
	      case make_list(a, b, c) of A -> B = binary_to_term(A),
					      [A, term_to_binary(B), B] end),
    io:format("proper list:~n ~p ->~n ~p ->~n ~p.~n",
	      case make_proper_list(a, b, c) of C -> D = binary_to_term(C),
						     [C, term_to_binary(D), D] end).
