-module(server).
-export([init/1, call/2, cast/2]).
-export([start/1]).     % helper function

call(Mod, Req) ->
    Mod ! {call, self(), Req},
    receive
        {Mod, Res} ->
            Res
    end.

cast(Mod, Req) ->
    Mod ! {cast, Req},
    ok.

init(Mod) ->
    register(Mod, self()),
    State = Mod:init(),
    loop__(Mod, State).

loop__(Mod, State) ->
    receive
        {call, From, Req} ->
            {Res, State2} = Mod:handle_call(Req, State),
            From ! {Mod, Res},
            loop__(Mod, State2);
        {cast, Req} ->
            State2 = Mod:handle_cast(Req, State),
            loop__(Mod, State2);
        Msg ->
            io:format("flushing unknown message from process ~p: ~p~n", [self(), Msg]),
            loop__(Mod, State)
    end.


% helper functions

start(Mod) ->
    io:format("starting module [~p]~n", [Mod]),
    spawn(?MODULE, init, [Mod]).
