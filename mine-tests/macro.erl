-module(macro).

-export([main/0]).

% erlc -Ddebug_log_macros macro.erl && erl -noshell -s macro main -s init stop

-include("macro.hrl").

main() ->
    ?log_msg0("alpha"),
    ?log_msg1("~p~n", [bravo]).
