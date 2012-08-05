
ERLANG_HOME = /usr/local/lib/erlang
#EI_VSN = 3.6.2
EI_VSN = 3.6.5

CFLAGS = -Wall -I../../../c -I$(ERLANG_HOME)/lib/erl_interface-$(EI_VSN)/include
LDFLAGS = -L$(ERLANG_HOME)/lib/erl_interface-$(EI_VSN)/lib
LIBS = -lbz2 -lz -lrt -lpthread -lm -lc -lerl_interface -lei

all: test

test: test.o
	gcc $(LDFLAGS) -o $@ $^ $(LIBS)

test.o: test.c
	gcc -Wall -c $(CFLAGS) -o $@ $<

../../../c/eterm.o ../../../c/str.o:
	$(MAKE) -C ../../../c

clean:
	rm -fv *.o

test.o: ../../../c/eterm.h
test: ../../../c/eterm.o ../../../c/str.o
