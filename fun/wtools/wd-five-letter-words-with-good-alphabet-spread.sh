#!/bin/bash

if [[ -z "$2" ]]; then
    echo "Usage: $0 <five-letter-word-1> <five-leter-word-2>"
    exit 1
fi

w1=$(echo "$1" | awk '{print $1}')
w2=$(echo "$2" | awk '{print $1}')

dict="$HOME/dr/fun/itasoftware-puzzles/word.lst"

s="$w1$w2"

for w in $(cat $dict | grep ^.....$ | grep -v "[$s]" | wd-nodup); do
    n=$(cat $dict | grep ^.....$ | grep -v "[$s$w]" | wd-nodup | wc -l | awk '{print $1}')

    if [[ $n -gt 0 ]]; then
        echo -n "$w1 $w2 $w: "
        cat $dict | grep ^.....$ | grep -v "[$s$w]" | wd-nodup | tr '\n' ' '
        echo "($n)"
    fi
done
