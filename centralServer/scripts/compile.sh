#!/usr/bin/env bash

cd ../src/

erlc ./user_logic.erl
erlc ./central_server.erl
erlc ./main_loop.erl
erlc ./crdt.erl

if [ -z $1 ]; then
    erl -eval "Server = central_server:start(1234)"
else
    erl -eval "Server = central_server:start($1)"
fi

cd ../scripts/
