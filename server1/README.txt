http://20bits.com/articles/erlang-a-generalized-tcp-server/


erlc *.erl

socket_server:start(echo_server, 7000, {echo_server, loop}).
