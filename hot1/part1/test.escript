#!/usr/bin/env escript
%% -*- erlang -*-

-define(debug_log_macros, true).
-include("$PWD/log.hrl").

%?log_msg1("~p", [M]),

main(_Args) ->
    make:files([eb_server]),

    {ok, _PID_eb_server} = eb_server:start_link(),

    ok = eb_server:create_account("alex"),

    {ok, 100} = eb_server:deposit("alex", 100),
    {ok, 90} = eb_server:withdraw("alex", 10),

    {error, not_enough_funds} = eb_server:withdraw("alex", 100),

    {ok, 110} = eb_server:deposit("alex", 20),
    ok = eb_server:delete_account("alex"),
    {error, account_does_not_exist} = eb_server:deposit("alex", 20),

    gen_event:stop(error_logger), % this flushes error logger messages
    io:format("~nok~n"),
    ok.
