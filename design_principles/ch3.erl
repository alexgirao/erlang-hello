-module(ch3).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([start_link/0, alloc/0, free/1]).     % helper functions (must use gen_server's call/cast)

% private functions

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

% callback functions

init(_Args) ->
    process_flag(trap_exit, true),      % let gen_server terminate upon exit(PID, normal)
    {ok, channels()}.

handle_call(alloc, _From, State) ->
    {Res, State2} = alloc(State),
    {reply, Res, State2}.

handle_cast({free, Ch}, State) ->
    State2 = free(Ch, State),
    {noreply, State2};

handle_cast(stop_me, State) ->
    {stop, normal, State}.          % gracefully stop, doesnt require trap_exit flag

terminate(normal, _State) ->        % required for gracefully stop
    io:format("terminating ~p~n", [self()]),
    ok.

% helper functions

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

alloc() ->
    gen_server:call(?MODULE, alloc).

free(Ch) ->
    gen_server:cast(?MODULE, {free, Ch}).
