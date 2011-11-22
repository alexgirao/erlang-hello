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
    % fun-info: {1,119830451,'-main/0-fun-1-'}
    fun() ->
           F = "~p~n",
           A = [bravo],
           true = is_list(F),
           true = is_list(A),
           error_logger:info_msg("~p:~p ~p: ~s",
                                 [macro,11,self(),io_lib:format(F, A)])
    end(),
    % fun-info: {2,114511903,'-main/0-fun-2-'}
    fun() ->
           F = "~p/~p~n",
           A = [charlie,delta],
           true = is_list(F),
           true = is_list(A),
           error_logger:info_msg("~p:~p ~p: ~s",
                                 [macro,12,self(),io_lib:format(F, A)])
    end().

module_info() ->
    erlang:get_module_info(macro).

module_info(X) ->
    erlang:get_module_info(macro, X).

