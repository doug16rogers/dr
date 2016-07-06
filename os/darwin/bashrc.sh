#!/bin/bash

alias md5sum="md5 -r "

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
