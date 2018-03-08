#!/bin/bash

# Specify the four words from a Mensa Puzzle Calendar word-replace-letter
# puzzle on the command line. This will do a brute force search for solutions
# over the ITA Software word list.

# From puzzle 20160928:

# Circle a letter in each word that can be changed to another letter to make
# a new word. The four letters you circle must be in different positions in
# the four words and you must use the same replacement letter for all four
# words. The letters you circle will spell a word. Use the same replacement
# letter with this word to make final four-letter word. The replacement
# letter used in puzzle 1 will be different from the one in puzzle 2. Can you
# find the final word for each puzzle?

# Puzzle 1: SLUM IONS CHIN MAKE
# Puzzle 2: SELF BRED GOOF GONE

script=`basename $0`

# @todo: Remove this restriction after adding code to derive permutations
#        from word length and/or count.
if [ $# -ne 4 ]; then
    echo "$script: specify four words on the command line"
    exit 1
fi

wordcount=4
wd[0]=`echo $1 | tr [:upper:] [:lower:]`
wd[1]=`echo $2 | tr [:upper:] [:lower:]`
wd[2]=`echo $3 | tr [:upper:] [:lower:]`
wd[3]=`echo $4 | tr [:upper:] [:lower:]`
echo "Looking for words: ${wd[0]} ${wd[1]} ${wd[2]} ${wd[3]}"

grepwd() {
    cat "$HOME/dr/fun/itasoftware-puzzles/word.lst" | grep $*
}

iswd() {
    grepwd "^$1$"  > /dev/null 2>&1
}

i=0
while [ $i -lt 4 ]; do
    word=${wd[i]}
    if [ ${#word} -ne ${#wd[0]} ]; then
        echo "$script: '$word' is not the same length as '${wd[0]}'."
        exit 1
    fi
    if ! iswd $word ; then
        echo "$script: '$word' is not in the ITA English word list."
        exit 1
    fi
    i=$[i+1]
done

wordlen=${#wd[0]}
# @todo: Remove this restriction after adding code to derive permutations
#        from word length and/or count.
if [ $wordlen -ne 4 ]; then
    echo "$script: only words of length 4 are supported."
    exit 1
fi

declare -a kPerm
kPerm[ 0]="0123"
kPerm[ 1]="0132"
kPerm[ 2]="0213"
kPerm[ 3]="0231"
kPerm[ 4]="0312"
kPerm[ 5]="0321"
kPerm[ 6]="1023"
kPerm[ 7]="1032"
kPerm[ 8]="1203"
kPerm[ 9]="1230"
kPerm[10]="1302"
kPerm[11]="1320"
kPerm[12]="2013"
kPerm[13]="2031"
kPerm[14]="2103"
kPerm[15]="2130"
kPerm[16]="2301"
kPerm[17]="2310"
kPerm[18]="3012"
kPerm[19]="3021"
kPerm[20]="3102"
kPerm[21]="3120"
kPerm[22]="3201"
kPerm[23]="3210"

# http://www.math.cornell.edu/~mec/2003-2004/cryptography/subs/frequencies.html:
#    e t a o i n s r h d l u c m f y w g p b v k x q j z
# http://www.letterfrequency.org/:
#    e t a o i n s r h l d c u m f p g w y b v k x j q z    # everyday speech and writing
#    e a r i o t n s l c u d p m h g b f y w k v x z j q    # Oxford dictionary
# Using the dictionary one since that's the kind of thing a puzzle will use.
found=0
which_letter=a
which_perm=0
for letter in e a r i o t n s l c u d p m h g b f y w k v x z j q; do
#    echo "Trying '$letter'."
    for ((which_perm = 0; which_perm < ${#kPerm[*]}; which_perm++)) do
        perm=${kPerm[which_perm]}
        permlen=${#perm}
        pat="^\("
        for ((pi = 0; pi < ${#perm}; pi++)) do
            word=${wd[pi]}
            char_index=${perm:pi:1}
            wl=${word:0:char_index}
            wr=${word:char_index+1:wordlen-char_index-1}
            pat="$pat$wl$letter$wr"
            if [ $pi -eq $[permlen - 1] ]; then
                pat="$pat\)$"
            else
                pat="$pat\|"
            fi
        done
        matches=`grepwd "$pat" 2> /dev/null | wc -l`
#        echo "$matches matches for \"$pat\"."
        if [ $matches -eq ${#perm} ]; then
            found=$[found + 1]
            pat_no_esc=`echo $pat | sed -e 's/[^a-z]/ /g'`
            pat_no_esc=`echo $pat_no_esc | sed -e 's/  / /g'`
            echo "Permutation $perm works with '$letter': $pat_no_esc"
            repword=""
            for ((k = 0; k < wordcount; k++)) ; do
                pk=${perm:k:1}
                repword="$repword${wd[k]:pk:1}"
            done
            echo "Replacement word: $repword"
            sols=()
            for ((k = 0; k < wordlen; k++)) ; do
                reppat="${repword:0:k}$letter${repword:k+1:wordcount-k-1}"
                sol=`grepwd "^$reppat$" 2> /dev/null`
                if [ ! -z "$sol" ]; then
                    sols[${#sols[*]}+1]=$sol
                fi
            done
            if [ ${#sols[*]} -eq 0 ]; then
                echo "No solution words found when substituting '$letter' anywhere in \"$repword\"."
            else
                echo "Solution words: ${sols[*]}"
            fi
        fi
    done
done

if [ $found -eq 0 ]; then
    exit 1
fi
exit 0
