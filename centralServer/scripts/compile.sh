#!/usr/bin/env bash

cd ../beam

erlc ../src/user_logic.erl
erlc ../src/central_server.erl
erlc ../src/main_loop.erl

if [ -z $1 ]; then
    erl -eval "central_server:start(1234)"
else
    erl -eval "central_server:start($1)"
fi

cd ../scripts/
