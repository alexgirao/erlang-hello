#!/usr/bin/env escript
%% -*- erlang -*-

-define(debug_log_macros, true).
-include("$PWD/log.hrl").

%?log_msg1("~p~n", [M]),

flush_unk() ->
    receive
	Msg ->
	    ?log_msg1("flushing unknown message from process ~p: ~p~n", [self(), Msg]),
	    flush_unk()
    after 0 ->
	    ok
    end.

main(_Args) ->
    %make:files([eb_server, eb_atm], [{d, debug_log_macros}]),
    make:files([eb_server, eb_atm]),

    {ok, _PID_eb_server} = eb_server:start_link(),
    {ok, PID_eb_atm} = eb_atm:start_link(),

    ok = eb_server:create_account("alex", 1234),

    ok = eb_atm:authorize("alex", 1234), % uses gen_fsm:send_event/2
    ok = eb_atm:deposit(50), % uses gen_fsm:send_event/2
    0 = receive wont_match -> 1 after 6000 -> 0 end, % let thank_you state timeout happen and state return to authorized

    ok = eb_atm:authorize("alex", 1234),
    ok = eb_atm:deposit(50), % uses gen_fsm:send_event/2
    thank_you = gen_fsm:sync_send_event(PID_eb_atm, {}),

    ok = eb_atm:authorize("alex", 1234), % uses gen_fsm:sync_send_event/2
    {ok, 70} = eb_atm:withdraw(30),
    eb_atm:cancel(), % thank_you -> unauthorized

    ok = eb_atm:authorize("alex", 1234),
    eb_atm:cancel(), % authorized -> unauthorized

    ok = eb_atm:deposit(100), % can't handle error, deposit uses asynchronous messages

    {error, invalid_message, {from, unauthorized}} = eb_atm:withdraw(30),

    flush_unk(),

    gen_event:stop(error_logger), % this flushes error logger messages
    io:format("~nok~n"),
    ok.
