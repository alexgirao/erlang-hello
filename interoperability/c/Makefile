
ERLANG_HOME = /usr/local/lib/erlang
#EI_VSN = 3.6.2
EI_VSN = 3.6.5

TOKYO_BUILDDIR = /home/alexgirao/build/tokyocabinet-1.4.44
HIGHERC_HOME = /home/alexgirao/projects/higherc

CFLAGS = -Wall -I$(TOKYO_BUILDDIR) -I$(HIGHERC_HOME) -I$(ERLANG_HOME)/lib/erl_interface-$(EI_VSN)/include
LDFLAGS = -static -L$(TOKYO_BUILDDIR) -L$(HIGHERC_HOME) -L$(ERLANG_HOME)/lib/erl_interface-$(EI_VSN)/lib
LIBS = -ltokyocabinet -lhigherc -lbz2 -lz -lrt -lpthread -lm -lc -lerl_interface -lei

all: test t2b b2t eterm.o

test t2b b2t: %: %.o eterm.o
	gcc $(LDFLAGS) -o $@ $< eterm.o $(LIBS)

eterm.o test.o t2b.o b2t.o: %.o: %.c
	gcc -Wall -c $(CFLAGS) -o $@ $<

clean:
	rm -fv *.o

test.o t2b.o b2t.o: eterm.h
