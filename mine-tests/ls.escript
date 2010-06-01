#!/usr/bin/env escript
%% -*- erlang -*-

% full list at $(SOURCE)/erts/emulator/beam/external.h
% more detailed info at $(DOC)/doc/erts-5.7.2/doc/html/part_frame.html

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

make_list_shorthand(A, B, C) ->
    iolist_to_binary(
      [
       <<?VERSION_MAGIC>>,
       <<?LIST_EXT,3:32/big-unsigned>>,   % list header with 3 items (32-bit big-endian and unsigned are defaults)
       t2b(A),
       t2b(B),
       t2b(C),
       <<?NIL_EXT>>   % every list must be followed by another term, NIL if no more items
      ]).

make_list_formal(A, B, C) ->
    iolist_to_binary(
      [
       <<?VERSION_MAGIC>>,
       <<?LIST_EXT,1:32>>, t2b(A),
       <<?LIST_EXT,1:32>>, t2b(B),
       <<?LIST_EXT,1:32>>, t2b(C),
       <<?NIL_EXT>>
      ]).

make_list_improper(A, B, C) ->  % tail is not a list, e.g.: [a|b]
    iolist_to_binary(
      [
       <<?VERSION_MAGIC>>,
       <<?LIST_EXT,1:32>>, t2b(A),
       <<?LIST_EXT,1:32>>, t2b(B),
       t2b(C)
      ]).

main(_) ->
    io:format("proper list, optimized/shorthand ([Term1,...,TermN]):~n ~p ->~n ~p ->~n ~p.~n",
	      case make_list_shorthand(a, b, c) of A -> B = binary_to_term(A),
					      [A, term_to_binary(B), B] end),
    io:format("proper list, formal/canonical ([Term1|[...|[TermN|[]]]]):~n ~p ->~n ~p ->~n ~p.~n",
	      case make_list_formal(a, b, c) of A2 -> B2 = binary_to_term(A2),
						     [A2, term_to_binary(B2), B2] end),
    io:format("improper list:~n ~p ->~n ~p ->~n ~p.~n",
	      case make_list_improper(a, b, c) of A3 -> B3 = binary_to_term(A3),
							[A3, term_to_binary(B3), B3] end),
    

    T = [c, d, [e | f]],

    io:format("~p = ~p~n", [T, term_to_binary(T)]),

    ok.
