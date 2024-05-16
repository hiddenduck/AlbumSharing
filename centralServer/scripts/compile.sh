#!/usr/bin/env bash

cd ../src/

erlc ./user_logic.erl
erlc ./central_server.erl
erlc ./main_loop.erl
erlc ./crdt.erl
erlc ./sessionManager.erl
erlc ./data_loop.erl

if [ -z $1 ]; then
    erl -eval "Server = central_server:start(8888, 8889)"
else
    erl -eval "Server = central_server:start($1, $2)"
fi

cd ../scripts/
