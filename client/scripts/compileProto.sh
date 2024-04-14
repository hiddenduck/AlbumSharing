#!/usr/bin/env bash

cd ../src/

protoc -I=. --go_out=. file.proto
