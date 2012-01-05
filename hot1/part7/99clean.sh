#!/bin/sh

find -iname '*.beam' | xargs rm -fv
rm -fv erl_crash.dump
git clean -d -f -x -- .
