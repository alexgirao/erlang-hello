#!/bin/sh

erlc 03test.erl
erl -noshell -boot eb_rel-1 -s 03test main -s init stop
