#!/usr/bin/env escript
%% -*- erlang -*-

-define(debug_log_macros, true).
-include("$PWD/log.hrl").

main(_Args) ->
    %make:files([eb_server, eb_atm], [{d, debug_log_macros}]),
    make:files([eb_server, eb_atm]),

    {ok, _PID_eb_server} = eb_server:start_link(),
    {ok, _PID_eb_atm} = eb_atm:start_link(),

    ok = eb_server:create_account("alex", 1234),
    ok = eb_atm:authorize("alex", 1234),

    %?log_msg1("~p~n", [1]),

    eb_atm:deposit(100),

    eb_atm:authorize("alex", 1234),
    eb_atm:withdraw(30),

    eb_atm:authorize("alex", 1234),
    eb_atm:cancel(),
    
    eb_atm:deposit(100),


    eb_atm:withdraw(30),


    gen_event:stop(error_logger), % this flushes error logger messages
    io:format("~nok~n"),
    ok.
