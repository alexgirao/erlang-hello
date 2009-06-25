#! /bin/sh

exec erl -noinput -detach -sname ensure_epmd_started@localhost -s erlang halt
