-module('loop-state').
-export([main/0, init/0]).

% reference: http://stackoverflow.com/questions/1669334/process-dictionary-or-loop-parameters
% erlc loop-state.erl && erl -noshell -s loop-state main -s init stop

-record(state, {
           listeners = [],
           new_var   = undefined,
           new_var2  = "" % same as []/nil
           % ...
        }).

init() ->
  loop(#state{}).

loop(State) ->
  receive
      {From, subscribe} ->
	  loop(State#state{listeners = [From | State#state.listeners]});
      {From, get_state} ->
	  From ! {state, State},
	  loop(State);
      somethingelse ->
	  % do_nothing(),
	  loop(State)
  end.

main() ->
    P = spawn(?MODULE, init, []),
    P ! {1, subscribe},
    P ! {2, subscribe},
    P ! {3, subscribe},
    P ! {self(), get_state},
    receive
	{state, S} ->
	    io:format("state: ~p~n", [S]);
	A ->
	    exit({error, A})
    end,
    ok.
