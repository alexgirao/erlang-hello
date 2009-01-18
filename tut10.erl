-module(tut10).
-export([convert_length/1]).

convert_length(Len) ->
    case Len of
	{centimeter, X} ->
	    {inch, X / 2.54};
	{inch, Y} ->
	    {centimeter, Y * 2.54}
    end.
