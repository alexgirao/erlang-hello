#!/bin/sh

set -u
set -e
set -x

erlc 0306test.erl

erl -noshell -boot eb_rel-1 -s 0306test main -s init stop
