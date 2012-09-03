
#
# . ${BER211_HOME}/profile
#
#

ERL_INTERFACE_INCLUDE = $(shell $(OTP_BIN)/erl -noshell -eval 'io:format("~s~n", [code:lib_dir(erl_interface, include)])' -s init stop)
ERL_INTERFACE_LIB = $(shell $(OTP_BIN)/erl -noshell -eval 'io:format("~s~n", [code:lib_dir(erl_interface, lib)])' -s init stop)

CFLAGS = -Wall -I$(ERL_INTERFACE_INCLUDE)
LDFLAGS = -L$(ERL_INTERFACE_LIB)

LIBS = -lbz2 -lz -lrt -lpthread -lm -lc -lerl_interface -lei

all: test t2b b2t eterm.o str.o

test t2b b2t: %: %.o eterm.o str.o
	gcc $(LDFLAGS) -o $@ $^ $(LIBS)

eterm.o str.o test.o t2b.o b2t.o: %.o: %.c
	gcc -Wall -c $(CFLAGS) -o $@ $<

clean:
	rm -fv *.o

str.o: str.h
test.o t2b.o b2t.o: eterm.h item.h
eterm.o: item.h
