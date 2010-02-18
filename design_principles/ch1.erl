-module(ch1).
-export([init/0, alloc/0, free/1]).
-export([start/0]).     % helper function

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

alloc() ->
    ?MODULE ! {alloc, self()},
    receive
        {?MODULE, Res} ->
            Res
    end.

free(Ch) ->
    ?MODULE ! {free, Ch},
    ok.

init() ->
    register(?MODULE, self()),
    Chs = channels(),
    loop__(Chs).

loop__(Chs) ->      % loop__ is private
    receive
        {alloc, From} ->
            {Ch, Chs2} = alloc(Chs),
            From ! {?MODULE, Ch},
            loop__(Chs2);
        {free, Ch} ->
            Chs2 = free(Ch, Chs),
            loop__(Chs2);
        Msg ->
            io:format("flushing unknown message from process ~p: ~p~n", [self(), Msg]),
            loop__(Chs)
    end.

% helper functions

start() ->
    spawn(?MODULE, init, []).
