#!/usr/bin/env bash

erlc ../src/user_logic.erl
erlc ../src/central_server.erl
erlc ../src/main_loop.erl

if [ -z $1 ]; then
    erl -pa ebin -eval "central_server:start(1234)"
else
    erl -pa ebin -eval "central_server:start($1)"
fi
