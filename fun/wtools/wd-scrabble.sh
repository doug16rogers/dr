#!/bin/bash

# Looks for words in the ITA word list using the letters given on the command
# line. Use '.' for a blank tile.

script=`basename $0`

if [ -z "$1" ]; then
    echo "Usage:"
    echo ""
    echo "  $script <letters>..."
    echo ""
    echo "$script finds all full-length words that match the given letters in each"
    echo "command line argument. Use '.' for a blank."
    echo ""
    exit 1
fi

# -----------------------------------------------------------------------------
grepwd() {
    cat "$HOME/dr/fun/itasoftware-puzzles/word.lst" | grep $*
}

# -----------------------------------------------------------------------------
checkword() {
    letters="$1"
    blanks=$2
    word="$3"
    wlen=${#word}
    llen=${#letters}
    blen=$[blanks + llen]
    if [ $wlen -ne $blen ]; then
        return 1
    fi
    # Brute force and slow.
    for ((i = 0; i < wlen; i++)) ; do
        wch="${word:i:1}"
        found=0
        for ((k = 0; k < ${#letters}; k++)) ; do
            lch="${letters:k:1}"
            if [ "$wch" == "$lch" ]; then
                letters="${letters:0:k}${letters:k+1:${#letters}-k-1}"
                found=1
                break
            fi
        done
        if [ $found -ne 1 ]; then
            if [ $blanks -gt 0 ]; then
                blanks=$[blanks - 1]
            else
                return 1
            fi
        fi
    done
    echo "$word"
    return 0
}   # checkword()

# -----------------------------------------------------------------------------
checkwords_stdin() {
    letters2="$1"
    blanks2=$2
    return_code=1       # No match found.
    while read word; do
        if checkword "$letters2" $blanks2 "$word"; then
            return_code=0
        fi
    done
    return $return_code
}   # checkwords_stdin()

# #############################################################################
while [ $# -gt 0 ]; do
    s=`echo "$1" | tr [:upper:] [:lower:]`
    shift
    slen=${#s}
    lenpat=""
    letpat=""
    blank_tiles=0
    for ((i = 0; i < slen; i++)) ; do
        lenpat="$lenpat."
        ch=${s:i:1}
        if [ $ch == "." ]; then
            blank_tiles=$[blank_tiles + 1]
        else
            letpat="$letpat$ch"
        fi
    done
    if [ $blank_tiles -gt 0 ]; then
        # If there is a blank then it's harder to reduce the search space, so
        # use all words of the full length.
        grepwd "^$lenpat$" | checkwords_stdin "$letpat" $blank_tiles
    else
        # If there's no blank then we can limit the matching by having grep
        # emit only words that have the letters given.
        gpat=""
        for ((i = 0; i < slen; i++)) ; do
            gpat="$gpat[$letpat]"
        done
        grepwd "^$gpat$" | checkwords_stdin "$letpat" $blank_tiles
    fi
done
