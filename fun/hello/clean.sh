#!/bin/bash

# Copyright (c) 2016-2019 Doug Rogers under the Zero Clause BSD License.
# You are free to do whatever you want with this software. See LICENSE.txt.

run () {
    cmd="$*"
    echo "$cmd"
    eval "$cmd"
}

run rm -f "*-hello" "*_hello" "*.o" "*.ali" "*.hi" "*~" "*.native" "*.log" "*.bf.c"
run rm -rf _build nimcache
