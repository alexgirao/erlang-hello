-module(echo).                 %% module name does not conflict with function name
-export([main/0]).

%% main function can be defined before needed functions
main() ->
	Pid = spawn(fun echo/0),                       %% syntactic sugar "fun() -> echo() end"

	register(echoprocess, Pid),                    %% register Pid with echoprocess atom

	whereis(echoprocess) ! {self(), now()},        %% whereis/1 usage, intended to be used in interactive shell
	echoprocess ! {self(), now()},                 %% without 'whereis/1' works the same way

	echoprocess ! {self(), done},

        true = lists:member(echoprocess, registered()),   %% registered() list registered processes as [atom]
        erlang:yield(),                                %% let echo process be done
        false = lists:member(echoprocess, registered()),

	flush_self().                                  %% process what was echoed

echo() ->
	receive
		{From, {A,B,C}} ->
			From ! {A,B,C},
			echo();
		{From, done} ->
			From ! done;
		Msg ->
			io:format("flushing unknown message from process ~p: ~p~n", [self(), Msg]),
			echo()
	end.

flush_self() ->
	receive
		done ->
			io:format("received: done~n"),
			ok;
		Msg ->
			io:format("received: ~p~n", [Msg]),
			flush_self()
	end.
