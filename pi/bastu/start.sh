#!/bin/bash

cd /usr/local/src/bastu
erl -sname bastu -pa ebin -pa /usr/local/src/erlang_ale/ebin /usr/local/src/erlang_ale/deps/*/ebin -run bastu
