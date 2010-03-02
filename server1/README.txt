
testing

    bash-3.2$ erlc *.erl
    bash-3.2$ erl
    Eshell V5.7.2  (abort with ^G)
    1> c(socket_server).
    {ok,socket_server}
    2> socket_server:start_link(echoserver01, 7000, {echo_server, loop}).
    {ok,<0.38.0>}
    3> whereis(echoserver01).
    <0.38.0>


reference: http://20bits.com/articles/erlang-a-generalized-tcp-server/
