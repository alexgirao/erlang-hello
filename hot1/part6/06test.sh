#!/bin/sh

set -e
set -x
set -u

ROOT="$(erl -noshell -eval 'io:format("~s~n", [code:root_dir()])' -s init stop)"

erlc 06test.erl
erl -noshell -boot "${ROOT}/releases/1/start" -s 06test main -s init stop
