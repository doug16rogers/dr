#!/bin/bash

if [ -z "$1" ]; then
    echo "ytaud.sh <youtube-url...>"
    exit 1
fi

while [ $# -gt 0 ]; do
    url="$1"
    shift
    youtube-dl --audio-format mp3 --extract-audio "$url"
done
