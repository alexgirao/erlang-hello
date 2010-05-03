
ERLANG_HOME = /usr/local/lib/erlang
EI_VSN = 3.6.2

CFLAGS = -I$(ERLANG_HOME)/lib/erl_interface-$(EI_VSN)/include
LDFLAGS = -L$(ERLANG_HOME)/lib/erl_interface-$(EI_VSN)/lib
LIBS = -lerl_interface -lei

all: test

test: test.o
	gcc $(LDFLAGS) -o $@ $< $(LIBS)

test.o: test.c
	gcc -Wall -c $(CFLAGS) -o $@ $<
