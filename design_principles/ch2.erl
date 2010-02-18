-module(ch2).
-export([init/0, handle_call/2, handle_cast/2]).
-export([start/0, alloc/0, free/1]).        % helper functions (must use server's call/cast)

channels() ->
    {_Allocated=[], _Free=lists:seq(1,100)}.

alloc({Allocated, [H|T]=_Free}) ->
    {H, {[H|Allocated], T}}.

free(Ch, {Alloc, Free}=Channels) ->
    case lists:member(Ch, Alloc) of
        true ->
            {lists:delete(Ch, Alloc), [Ch|Free]};
        false ->
            io:format("warning: ~p not allocated~n", [Ch]),
            Channels
    end.

init() ->
    channels(). % server state

handle_call(alloc, State) ->
    alloc(State).

handle_cast({free, Ch}, State) ->
    free(Ch, State).

% helper functions

start() ->
    server:start(?MODULE).

alloc() ->
    server:call(?MODULE, alloc).

free(Ch) ->
    server:cast(?MODULE, {free, Ch}).
