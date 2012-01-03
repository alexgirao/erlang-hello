-module(code_lock).
-behaviour(gen_fsm).

-export([init/1, terminate/3, handle_event/3, locked/2, open/2]).    % init, terminate and state names
-export([start_link/1, button/1, stop/0]).  % helper functions

% private functions

do_lock() ->
    io:format("lock done~n").

do_unlock() ->
    io:format("lock undone~n").

% callback functions

init(Code) ->
    process_flag(trap_exit, true),
    {ok, locked, {[], Code}}.

locked({button, Digit}, {SoFar, Code}) -> % StateName(Event, StateData)
    case SoFar ++ [Digit] of
        Code ->
            do_unlock(),
            {next_state, open, {[], Code}, 3000};   % {next_state, StateName, StateData, Timeout}
        Incomplete when length(Incomplete) < length(Code) ->
            {next_state, locked, {Incomplete, Code}};   % {next_state, StateName, StateData}
        Wrong ->
            io:format("wrong code [~p] != [~p]~n", [Wrong, Code]),
            {next_state, locked, {[], Code}}
    end.

open(timeout, State) -> % StateName(timeout, StateData) (timeout from state transition)
    do_lock(),
    {next_state, locked, State}.

handle_event(stop, _StateName, StateData) ->        % handle gen_fsm:send_all_state_event(FsmRef, Event)
    {stop, normal, StateData}.

terminate(normal, _StateName, _StateData) ->
    ok.

% helper functions

start_link(Code) ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, Code, []).

button(Digit) ->
    gen_fsm:send_event(?MODULE, {button, Digit}).

stop() ->
    gen_fsm:send_all_state_event(?MODULE, stop).
