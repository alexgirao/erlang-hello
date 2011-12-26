-file("./macro.erl", 1).

-file("./macro.hrl", 1).

-file("./macro.erl", 8).

main() ->
    % fun-info: {0,101837112,'-main/0-fun-0-'}
    fun() ->
           true = is_list("alpha"),
           error_logger:info_msg("~p:~p ~p: ~s",
                                 [macro,10,self(),"alpha"])
    end(),
    % fun-info: {1,50681794,'-main/0-fun-1-'}
    fun() ->
           F_log = "~p~n",
           A_log = [bravo],
           true = is_list(F_log),
           true = is_list(A_log),
           error_logger:info_msg("~p:~p ~p: ~s",
                                 [macro,
                                  11,
                                  self(),
                                  io_lib:format(F_log, A_log)])
    end(),
    % fun-info: {2,119721388,'-main/0-fun-2-'}
    fun() ->
           F_log = "~p/~p~n",
           A_log = [charlie,delta],
           true = is_list(F_log),
           true = is_list(A_log),
           error_logger:info_msg("~p:~p ~p: ~s",
                                 [macro,
                                  12,
                                  self(),
                                  io_lib:format(F_log, A_log)])
    end().

module_info() ->
    erlang:get_module_info(macro).

module_info(X) ->
    erlang:get_module_info(macro, X).

