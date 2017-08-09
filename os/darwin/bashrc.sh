#!/bin/bash

export CUDA_HOME=/usr/local/cuda
export DYLD_LIBRARY_PATH="$DYLD_LIBRARY_PATH:$CUDA_HOME/lib"
export PATH="$CUDA_HOME/bin:$PATH"

alias ldd="otool -L "
alias md5sum="md5 -r "

alias gdb='echo "This is an alias for lldb..."; lldb '

eject() {
    if [ -z "$1" ]; then
        echo "usage: eject <volume>..."
        exit 1
    fi
    while [ $# -gt 0 ]; do
        ejvol="$1"
        shift
        for mnt in /Volumes/*; do
            mvol=`echo "$mnt" | sed -e 's:/Volumes/::'`
            if echo "$mvol" | grep -i "$ejvol" > /dev/null 2>&1; then
                cmd="osascript -e 'tell application \"Finder\" to eject \"$mvol\"'"
                echo "$cmd"
                eval "$cmd"
                break
            fi
        done
    done
}
