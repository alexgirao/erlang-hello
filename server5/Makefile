top = ../..

#vpath %.erl $(top)/fcgi/src

sources = file_handle_cache.erl tcp_acceptor.erl tcp_acceptor_sup.erl	\
 tcp_listener.erl tcp_listener_sup.erl rabbit_misc.erl test-0.erl	\
 test-1.erl

beams = $(patsubst %.erl, %.beam, $(sources))

all: $(beams)

%.beam: %.erl
	erlc +debug_info $<

clean:
	rm -fv *.beam
