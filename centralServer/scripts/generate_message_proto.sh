#!/usr/bin/env bash

cd ../src/gpb

make

cd ../proto_generated

../gpb/bin/protoc-erl -I. ../../protobuf/message.proto

mv ../../protobuf/message.erl ../../protobuf/message.hrl .

erlc -I../gpb/include message.erl