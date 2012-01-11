#!/usr/bin/env escript
%% -*- erlang -*-

-define(debug_log_macros, true).
-include("$PWD/macro.hrl").

test(ExprString) ->
    ?log_msg1("Doing ~p", [ExprString]),

    {ok, Tokens, _EndLocation} = erl_scan:string(ExprString),
    ?log_msg1("Tokens=~p", [Tokens]),

    % way 1

    {ok, Term0} = erl_parse:parse_term(Tokens),
    ?log_msg1("Term0=~p", [Term0]),

    % get abstract forms of the parsed expressions

    {ok, [Expr]=ExprList} = erl_parse:parse_exprs(Tokens),

    ?log_msg1("ExprList=~p, Expr=~p", [ExprList, Expr]),

    % way 2

    {value, Term1, _NewBindings} = erl_eval:exprs(ExprList, []),
    ?log_msg1("Term1=~p", [Term1]),

    % way 3

    {value, Term2, _NewBindings} = erl_eval:expr(Expr, []),
    ?log_msg1("Term2=~p", [Term2]),

    % verify

    Term0 = Term1,
    Term0 = Term2,

    %

    ok.

main(_Args) ->

    ok = test("1."),
    ok = test("{test}."),

    %

    gen_event:stop(error_logger), % this flushes error_logger messages from escript processes
    io:format("~nok~n"),

    ok.
