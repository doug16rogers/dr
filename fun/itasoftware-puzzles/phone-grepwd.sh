#!/bin/bash

# Prints all uncapitalized English words that match a phone number, also
# breaking it into 2 or 3 words.

# Use --inexact to search for words that might be longer than the pattern of
# digits. Note that --inexact does not apply to multi-word searches.

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

inexact=0

if [ "$1" = "--inexact" ]; then
    shift
    inexact=1
fi

number="$*"

if [ -z "$number" ]; then
    echo "usage: phone-grepwd.sh [--exact] <digits...>"
    exit 1
fi

i=0
regex_i=0
while [ $i -lt ${#number} ]; do
    letter=${number:$i:1}
    is_digit=0
    is_letter=0
    case $letter in
        [0-9])
            is_digit=1
            ;;
        [A-Za-z])
            is_letter=1
            ;;
        *)
    esac

    if [ $is_digit -eq 1 ]; then
        regex[$regex_i]="[${letters[$letter]}]"
        regex_i=$[regex_i+1]
    elif [ $is_letter -eq 1 ]; then
        regex[$regex_i]="$letter"
        regex_i=$[regex_i+1]
    fi
    i=$[i + 1]
done

re=`echo "${regex[@]}" | sed 's/ //g'`

if [ $inexact -eq 0 ]; then
    re="^$re\$"
fi

# Run the full number through the grepper.
echo "# regex=$re"
words=(`grep "$re" "$HOME/dr/fun/itasoftware-puzzles/word.lst"`)
if [ ${#words[@]} -ne 0 ]; then
    echo "(${words[@]})"
fi

n=${#regex[@]}

# Break into pairs of regexes only if at least 4 letters.
if [ $n -ge 4 ]; then
    # Now break the full number into all possible pairs of regexes (m=2).
    m=2
    for ((i = 1; i < n; i++)) do
        k=0
        for ((j = 0; j < m; j++)) do
            re[$j]=""
            words[$j]=""
            grep_result[$j]=0
        done
        for ((j = 0; j < n; j++)) do
            if [ $j -eq $i ]; then
                k=$[k+1]
            fi
            re[$k]="${re[$k]}${regex[$j]}"
        done
        # echo "# i=$i re=${re[@]}"
        all_found=1
        for ((j = 0; j < m; j++)) do
            wds=(`grep "^${re[$j]}$" "$HOME/dr/fun/itasoftware-puzzles/word.lst"`)
            if [ ${#wds[@]} -eq 0 ]; then
                all_found=0
                break
            fi
            words[$j]="${wds[@]}"
        done
        if [ $all_found -eq 1 ]; then
            # echo "# re=${re[@]}"
            echo "(${words[0]}) (${words[1]}) "
        fi
    done
fi            

# Break into trios of regexes only if at least 5 letters.
if [ $n -ge 5 ]; then
    # Now break the full number into all possible trios of regexes (m=3).
    m=3
    for ((i = 1; i < n; i++)) do
        for ((p = i+1; p < n; p++)) do
            k=0
            for ((j = 0; j < m; j++)) do
                re[$j]=""
                words[$j]=""
                grep_result[$j]=0
            done
            for ((j = 0; j < n; j++)) do
                if [ $j -eq $i ]; then
                    k=$[k+1]
                fi
                if [ $j -eq $p ]; then
                    k=$[k+1]
                fi
                re[$k]="${re[$k]}${regex[$j]}"
            done
            # echo "# i=$i p=$p re=${re[@]}"
            all_found=1
            for ((j = 0; j < m; j++)) do
                wds=(`grep "^${re[$j]}$" "$HOME/dr/fun/itasoftware-puzzles/word.lst"`)
                if [ ${#wds[@]} -eq 0 ]; then
                    all_found=0
                    break
                fi
                words[$j]="${wds[@]}"
            done
            if [ $all_found -eq 1 ]; then
                # echo "# re=${re[@]}"
                echo "(${words[0]}) (${words[1]}) (${words[2]}) "
            fi
        done   # p
    done   # i
fi            
