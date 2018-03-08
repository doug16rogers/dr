#!/bin/bash

run () {
    cmd="$*"
    echo "$cmd"
    eval "$cmd"
}

run rm -f "*-hello" "*_hello" "*.o" "*.ali" "*.hi" "*~" "*.native" "*.log" "*.bf.c"
run rm -rf _build nimcache
