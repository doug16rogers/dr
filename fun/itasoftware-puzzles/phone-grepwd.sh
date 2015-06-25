#!/bin/bash

# Prints all uncapitalized English words that match a phone number.

# Use --exact to limit the search to words that have the same length
# as the digits. Otherwise any words that contain matching digits
# will be printed. For example, without --exact, 4357 will match
# "help", "helpful", "unhelpful", etc.

# Any non-digit letters are passed through to the regular expression, so you
# may emulate --exact with '^<digits>$'.

# This will match 'o' for zero and 'i' for one.

letters[0]="o"
letters[1]="i"
letters[2]="abc"
letters[3]="def"
letters[4]="ghi"
letters[5]="jkl"
letters[6]="mno"
letters[7]="pqrs"
letters[8]="tuv"
letters[9]="wxyz"

exact=0

if [ "$1" = "--exact" ]; then
    shift
    exact=1
fi

number="$*"

if [ -z "$number" ]; then
    echo "usage: phone-grepwd.sh [--exact] <digits...>"
    exit 1
fi

regex=""
i=0
while [ $i -lt ${#number} ]; do
    letter=${number:$i:1}
    is_digit=0
    case $letter in
        [0-9])
            is_digit=1
            ;;
        *)
    esac

    if [ $is_digit -eq 1 ]; then
        regex="${regex}[${letters[$letter]}]"
    else
        regex="${regex}$letter"
    fi
    i=$[i + 1]
done

if [ $exact -eq 1 ]; then
    regex="^$regex\$"
fi

echo "regex=$regex"
cat "$HOME/dr/fun/itasoftware-puzzles/word.lst" | grep "$regex"
