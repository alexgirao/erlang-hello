#!/bin/sh
#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1996-2009. All Rights Reserved.
# 
# The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved online at http://www.erlang.org/.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
# 
# %CopyrightEnd%
#
# This program invokes the erlang emulator by calling run_erl.
# It should only be used at an embedded target system.
# It should be modified to give the correct flags to erl (via start_erl),
# e.g -mode embedded -sname XXX
#
# Usage: start [Data]
#
ROOTDIR="$(erl -noshell -eval 'io:format("~s~n", [code:root_dir()])' -s init stop)"

if [ -z "$RELDIR" ]
then
   RELDIR=$ROOTDIR/releases
fi

START_ERL_DATA=${1:-$RELDIR/start_erl.data}

#$ROOTDIR/bin/run_erl -daemon /tmp/ $ROOTDIR/log "exec $ROOTDIR/bin/start_erl $ROOTDIR $RELDIR $START_ERL_DATA"
$ROOTDIR/bin/start_erl $ROOTDIR $RELDIR $START_ERL_DATA -mode embedded
