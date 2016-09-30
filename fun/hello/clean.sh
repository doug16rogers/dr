#!/bin/bash

run () {
    cmd="$*"
    echo "$cmd"
    eval "$cmd"
}

run rm -f "*-hello" "*.o" "*.ali" "*.hi" "*~" "*.native" "*.log"
run rm -rf _build
