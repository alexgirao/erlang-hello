#!/bin/sh

set -u
set -e
set -x

erlc 0306test.erl

ROOT="$(erl -noshell -eval 'io:format("~s~n", [code:root_dir()])' -s init stop)"

erl -noshell -boot "${ROOT}/releases/2/start" -s 0306test main -s init stop
