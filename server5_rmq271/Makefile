top = ../..

#vpath %.erl $(top)/fcgi/src

sources = gen_server2.erl file_handle_cache.erl tcp_acceptor.erl	\
 tcp_acceptor_sup.erl tcp_listener.erl tcp_listener_sup.erl		\
 rabbit_misc.erl test-0.erl test-1.erl priority_queue.erl

beams = $(patsubst %.erl, %.beam, $(sources))

all: $(beams)

%.beam: %.erl
	erlc +debug_info -pa . $<

clean:
	rm -fv *.beam
