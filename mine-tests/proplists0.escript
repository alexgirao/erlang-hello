#!/usr/bin/env escript
%% -*- erlang -*-

-define(debug_log_macros, true).
-include("$PWD/macro.hrl").

main(_Args) ->

    D = [{description,"ErlyBank system."},
	 {id,[]},
	 {vsn,"1.0"},
	 {modules,[eb_app,eb_sup,eb_server,eb_atm,eb_event_manager,
		   eb_withdrawal_handler]},
	 {maxP,infinity},
	 {maxT,infinity},
	 {registered,[eb_sup,eb_server,eb_atm,eb_event_manager]},
	 {included_applications,[]},
	 {applications,[kernel,stdlib,sasl]},
	 {env,[{included_applications,[]}]},
	 {mod,{eb_app,[]}},
	 {start_phases,undefined}
	],

    undefined = proplists:get_value('not present atom', D),

    ?log_msg1("vsn=~p", [proplists:get_value(vsn, D)]),
    ?log_msg1("applications=~p", [proplists:get_value(applications, D)]),

    %

    gen_event:stop(error_logger), % this flushes error_logger messages from escript processes
    io:format("~n"),

    ok.
