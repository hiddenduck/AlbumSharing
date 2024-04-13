#!/bin/bash

docker run --name postgres -e POSTGRES_PASSWORD=postgres -p 5432:5432 -d postgres:16

sleep 5

docker exec -i postgres psql -U postgres < createDB.sql


