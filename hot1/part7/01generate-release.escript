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

get_app_key(App, Key) ->
    {ok, D} = application:get_all_key(App),
    proplists:get_value(Key, D).

main(_Args) ->
    true = code:add_patha("ebin"),  % prepend 'ebin' dir, so we load this application before the one already deployed

    ok = application:load(erlybank),

    Apps = get_app_key(erlybank, applications),

    ?log_msg1("Apps=~p", [Apps]),

    lists:foreach(fun (I) -> application:load(I) end, Apps),

    Provides = [{X, get_app_key(X, vsn)} || X <- Apps ++ [erlybank]],

    ?log_msg1("Provides=~p", [Provides]),

    %

    Rel1 = {release,
	   {"eb_rel", "1"},
	   {erts, erlang:system_info(version)},
	   Provides
	  },

    Rel2 = {release,
	   {"eb_rel", "2"},
	   {erts, erlang:system_info(version)},
	   Provides
	  },

    ok = file:write_file("eb_rel-1.rel", io_lib:format("~p.~n", [Rel1])),
    ok = file:write_file("eb_rel-2.rel", io_lib:format("~p.~n", [Rel2])),

    %

    flush_unk(),
    gen_event:stop(error_logger), % this flushes error logger messages
    io:format("~nok~n"),
    ok.
