#!/bin/bash

erl -pa ./lib/ejson-0.1.0/ebin -pa ebin/ -noshell -run db_clone run $1
