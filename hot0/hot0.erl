-module(hot0).

-export([
	 start/0,
	 loop/0,
	 init/0
	]).

-define(V, 4).

start() ->
    spawn(?MODULE, init, []).

init() ->
    process_flag(trap_exit, true),
    ?MODULE:loop().

loop() ->
     receive
         print_version ->
             io:format("~p current version is ~p~n", [?MODULE, ?V]),
             ?MODULE:loop();
         Other ->
             io:format("~p got message ~p~n", [?MODULE, Other]),
             ?MODULE:loop()
     end.
