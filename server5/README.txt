
based on rabbitmq-server-2.2.0

**** concurrent accept: how it works? what are the advantages?

    in tcp_listener.erl, there is an option to start multiple acceptor
    processes, such processes will each invoke prim_inet:async_accept
    for the same listening socket, tracking down the calls i got to
    the port command in
    otp_src_R14B01/erts/preloaded/src/prim_inet.erl which calls
    erlang:port_control(LSocket, ?TCP_REQ_ACCEPT, [Time]) which in
    turn may call tcp_inet_ctl in
    otp_src_R14B01/erts/emulator/drivers/common/inet_drv.c, from a
    basic reading of the code, it seems that the port driver enqueue
    accepts when switch from state TCP_STATE_ACCEPTING to
    TCP_STATE_MULTI_ACCEPTING.

**** testing on erlang shell

    ERL_LIBS="$(pwd)" erl -boot start_sasl
    1> make:files([file_handle_cache, rabbit_misc, tcp_acceptor, tcp_acceptor_sup, tcp_listener, tcp_listener_sup, 'test-0', 'test-1']).
    2> spawn('test-1', main, []).

**** testing with escript

    escript test-2.escript

****
