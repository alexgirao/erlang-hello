http://20bits.com/articles/erlang-a-generalized-tcp-server/

    echo_server:start().        % listen on 7000

    or

    socket_server:start(echo_server, 7001, {echo_server, loop}).
