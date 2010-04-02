-module(socket_server).
-author('Jesse E.I. Farmer <jesse@20bits.com>').
-behavior(gen_server2).

-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([worker_loop/1]).
-export([start_link/3]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-record(server_state, {
        port,
        loop,
        ip=any,
        lsocket=null}).

start_link(Name, Port, Loop) ->
    State = #server_state{port = Port, loop = Loop},
    gen_server2:start_link({local, Name}, ?MODULE, State, []).

init(State = #server_state{port=Port}) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),     % this call won't block
    NewState = State#server_state{lsocket=LSocket},
    {ok, spawn_worker(NewState)}.

handle_cast({accepted, _Pid}, State=#server_state{}) ->
    {noreply, spawn_worker(State)}.

worker_loop({Server, LSocket, {M, F}}) ->
    {ok, Socket} = gen_tcp:accept(LSocket),     % this call will block (infinity timeout)

    % tell the gen_server2 process to spawn a new worker
    % process to accept future incoming connections
    gen_server2:cast(Server, {accepted, self()}),

    % transfer control to worker module
    M:F(Socket).
   
spawn_worker(State = #server_state{lsocket=LSocket, loop=Loop}) ->
    proc_lib:spawn_link(?MODULE, worker_loop, [{self(), LSocket, Loop}]),
    State.

% These are just here to suppress warnings.
handle_call(_Msg, _Caller, State) -> {noreply, State}.
handle_info(_Msg, Library) -> {noreply, Library}.
terminate(_Reason, _Library) -> ok.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.
