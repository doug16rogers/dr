#!/bin/bash

# Some of this taken from https://www.linuxjournal.com/content/bash-arrays:

# Sample initialization for 4x8:
arr=(this is not a test)

echo 'arr=(00000000 0000000 000000000 00000000)'
echo 'All items in array    ${arr[*]}  = ' ${arr[*]}
echo 'All indexes in array  ${!arr[*]} = ' ${!arr[*]}
echo 'Length of array       ${#arr[*]} = ' ${#arr[*]}
echo 'Length of first item  ${#arr[0]} = ' ${#arr[0]}

ROWS=8
COLS=8
blank_row=`printf %${COLS}s ' '`
unset grid
for ((r = 0; r < ROWS; ++r)); do
    grid[$r]="$blank_row"
done

print_grid() {
    for ((r = 0; r < ${#grid[*]}; ++r)); do
        echo "|${grid[$r]}|"
    done
}

set_cell() {
    r=$1
    c=$2
    v=$3
    if [ $r -ge 0 -a $r -lt $ROWS -a $c -ge 0 -a $c -lt $COLS ]; then
        row="${grid[$r]}"
        new="${row:0:$[c]}$v${row:$[c+1]:${#row}}"
        grid[$r]="$new"
    fi
}

# This allows for wrapping so you can use (small) negative numbers here.
set_cell() {
    r=$1
    r=$[(r + ROWS) % ROWS]
    c=$2
    c=$[(c + COLS) % COLS]
    v=$3
    row="${grid[$r]}"
    new="${row:0:$[c]}$v${row:$[c+1]:${#row}}"
    grid[$r]="$new"
}

get_cell() {
    r=$1
    r=$[(r + ROWS) % ROWS]
    c=$2
    c=$[(c + COLS) % COLS]
    row="${grid[$r]}"
    echo ${row:$c:$c}
}

echo "Initially:"
print_grid

set_cell 3 6 X
echo "After 'set_cell 3 6 X':"
print_grid

echo 'get_cell 2 4 = "'`get_cell 2 4`'"'
echo 'get_cell 3 6 = "'`get_cell 3 6`'"'
