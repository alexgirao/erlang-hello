
ERLANG_HOME = /usr/local/lib/erlang
#EI_VSN = 3.6.2
EI_VSN = 3.6.5

CFLAGS = -Wall -I$(ERLANG_HOME)/lib/erl_interface-$(EI_VSN)/include
LDFLAGS = -L$(ERLANG_HOME)/lib/erl_interface-$(EI_VSN)/lib
LIBS = -lbz2 -lz -lrt -lpthread -lm -lc -lerl_interface -lei

all: test t2b b2t eterm.o str.o

test t2b b2t: %: %.o eterm.o str.o
	gcc $(LDFLAGS) -o $@ $^ $(LIBS)

eterm.o str.o test.o t2b.o b2t.o: %.o: %.c
	gcc -Wall -c $(CFLAGS) -o $@ $<

clean:
	rm -fv *.o

str.o: str.h
test.o t2b.o b2t.o: eterm.h
