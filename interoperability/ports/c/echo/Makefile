
all: test

test: test.o
	gcc -o $@ $<

test.o: test.c
	gcc -Wall -c -o $@ $<
