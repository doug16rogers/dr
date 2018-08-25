#!/bin/bash

# Implementation of John Horton Conway's Game of Life in bash just to show
# some examples of bash array usage, etc.

LOG=jhc_life.log
rm -f $LOG

# Bash doesn't directly support multi-dimensional arrays but there are many
# kluges to work around that, like using "#,#' strings as array indexes. I've
# chosen to use a one-dimensional array of strings.

# ROWS=24
# COLS=80
DEAD="."
LIVE="#"
ROWS=16
COLS=40

# All setting/getting is relative to ROW, COL except for the _abs_ functions.
ROW=0
COL=0
ROW_STACK=()
COL_STACK=()

grid=()

# These allows for wrapping so you can use (small) negative numbers here.

# grid_abs_set row col cell-value
grid_abs_set() {
    local r=$1
    r=$[(r + ROWS) % ROWS]
    local c=$2
    c=$[(c + COLS) % COLS]
    local v=$3
    if [[ -z "$v" ]]; then
        v="$LIVE"
    fi
    local row="${grid[$r]}"
    local new="${row:0:$[c]}$v${row:$[c+1]:$[${#row}-1]}"
    grid[$r]="$new"
}

# grid_abs_get row col
grid_abs_get() {
    local r=$1
    r=$[(r + ROWS) % ROWS]
    local c=$2
    c=$[(c + COLS) % COLS]
    row="${grid[$r]}"
    printf "%1s" "${row:$c:1}"
}

# grid_set row-delta col-delta cell-value
grid_set() {
    local r=$1
    local c=$2
    grid_abs_set $[ROW + r] $[COL + c] "$3"
}

# grid_set_list r c ...
grid_set_list() {
    while [[ $# -ge 2 ]]; do
        grid_set $1 $2
        shift 2
    done
}

# grid_get row-delta col-delta
grid_get() {
    local r=0
    local c=0
    if [[ ! -z "$1" ]]; then r=$1; fi
    if [[ ! -z "$2" ]]; then c=$2; fi
    val=`grid_abs_get $[ROW + r] $[COL + c]`
    printf "%s" "$val"
}

# flip_cell row col
grid_flip_cell() {
    r=$1
    c=$2
    # echo -n flipping $1 $2 - grid_abs_get is "|`grid_abs_get $r $c`| " >> $LOG
    if [[ "`grid_abs_get $r $c`" = "$LIVE" ]]; then
        # echo setting dead >> $LOG
        grid_abs_set $r $c "$DEAD"
    else
        # echo setting live >> $LOG
        grid_abs_set $r $c "$LIVE"
    fi
}

# grid_goto row col
# Used for relative set/get functions above.
grid_goto() {
    local r=$1
    local c=$2
    if [[ -z "$r" ]]; then
        r=0
    fi
    if [[ -z "$c" ]]; then
        c=0
    fi
    ROW=$r
    COL=$c
}

# grid_move drow dcol
# Change ROW,COL by drow,dcol
grid_move() {
    local dr=$1
    local dc=$2
    if [[ -z "$r" ]]; then
        dr=0
    fi
    if [[ -z "$c" ]]; then
        dc=0
    fi
    ROW=$[ROW + dr]
    COL=$[COL + dc]
    # echo grid_moved to ROW=$ROW COL=$COL >> $LOG
}

# Save the existing ROW,COL on the stack.
grid_push() {
    depth=${#ROW_STACK[@]}
    # echo pushing ROW=$ROW COL=$COL depth=$depth >> $LOG
    ROW_STACK[$depth]=$ROW
    COL_STACK[$depth]=$COL
}

# Restore ROW,COL from the stack.
grid_pop() {
    local depth=$[${#ROW_STACK[@]}-1]
    ROW=${ROW_STACK[$depth]}
    unset ROW_STACK[$depth]
    COL=${COL_STACK[$depth]}
    unset COL_STACK[$depth]
    # echo popped ROW=$ROW COL=$COL >> $LOG
}

# grid_place_block ul-row ul-col
grid_place_block() {
    grid_set  0  0
    grid_set  0 +1
    grid_set +1  0
    grid_set +1 +1
}

# grid_place_vertical_blinker orientation
# orientation may be "h" (horizontal) or "v" (vertical).
grid_place_blinker() {
    if [[ "$1" = "v" ]]; then
        grid_set -1 0
        grid_set  0 0
        grid_set +1 0
    else
        grid_set 0 -1
        grid_set 0  0
        grid_set 0 +1
    fi
}

# grid_place_glider mid-row mid-col [orientation]
# orientation may be "se" "sw" "ne" "nw", defaulting to "se"
# @todo: Add a phase option?
grid_place_glider() {
    local orientation=$1
    if [[ -z "$orientation" ]]; then
        orientation=se;
    fi
    if [[ "$orientation" = "nw" ]]; then
        grid_set -1 -1
        grid_set -1  0
        grid_set -1 +1
        grid_set  0 -1
        grid_set +1  0
    elif [[ "$orientation" = "ne" ]]; then
        grid_set -1  0
        grid_set -1 +1
        grid_set  0 -1
        grid_set  0 +1
        grid_set +1 +1
    elif [[ "$orientation" = "sw" ]]; then
        grid_set -1 -1
        grid_set  0 -1
        grid_set  0 +1
        grid_set +1 -1
        grid_set +1  0
    else
        grid_set -1  0
        grid_set  0 +1
        grid_set +1 -1
        grid_set +1  0
        grid_set +1 +1
    fi
}

# grid_place_spaceshit [orientation]
# orientation should be u d l r, defaulting to r.
grid_place_spaceship() {
    local orientation=$1
    if [[ -z "$orientation" ]]; then orientation=r; fi
    if [[ "$orientation" = "u" ]]; then
        grid_set_list -2 -1 -2 0 -2 +1  -1 -1 -1 +2  0 -1  +1 -1  +2 0 +2 +2
    elif [[ "$orientation" = "l" ]]; then
        grid_set_list -2 -1 -2 +2  -1 -2  0 -2 0 +2  +1 -2 +1 -1 +1 0 +1 +1
    elif [[ "$orientation" = "d" ]]; then
        grid_set_list -2 -2 -2 0  -1 +1  0 +1  +1 -2 +1 +1  +2 -1 +2 0 +2 +1
    else
        grid_set_list -1 -1 -1 0 -1 +1 -1 +2  0 -2 0 +2  +1 +2  +2 -2 +2 +1
    fi
}

# grid_place_glider_gun [orientation]
# orientation may be "se" "sw" "ne" "nw", defaulting to "se"
# @todo: Support all orientations.
grid_place_glider_gun() {
    local orientation=$3
    if [[ -z "$orientation" ]]; then
        orientation=se;
    fi
    grid_push; grid_move  0 -19; grid_place_block; grid_pop
    grid_push; grid_move -2  17; grid_place_block; grid_pop
    grid_push; grid_move +1  -2; grid_place_blinker v; grid_pop
    grid_push; grid_move +1  -8; grid_place_blinker v; grid_pop
    grid_push; grid_move -1  +2; grid_place_blinker v; grid_pop
    grid_push; grid_move -1  +3; grid_place_blinker v; grid_pop
    grid_set_list  -2 -6  -2 -5  -1 -7  -1 -3
    grid_set_list  +1 -4  +1 -1  +3 -7  +3 -3
    grid_set_list  +4 -6  +4 -5  -3 +4  +1 +4
    grid_set_list  -4 +6  -3 +6  +1 +6  +2 +6
}

# grid_print [message]
grid_print() {
    if [[ -n "$1" ]]; then
        echo "$@"
        # echo "$@" >> $LOG
    fi
    for ((r = 0; r < ROWS; ++r)); do
        echo "|${grid[$r]}|" | tr "$DEAD" " "
    done
}

# grid_clear_print [message]
grid_clear_print() {
    clear
    grid_print $@
}

# grid_init [char-for-each-cell]
grid_init() {
    local char=$1
    if [[ -z "$char" ]]; then
        char="$DEAD"
    fi
    char=${char:0:1}
    local initialized_row=""
    for ((c = 0; c < COLS; c++)); do
        initialized_row="${initialized_row}$char"
    done
    for ((r = 0; r < ROWS; r++)); do
        grid[$r]="$initialized_row"
    done
}

# grid_randomly_flip [permil-chance-of-flip]
# Default is 10 (1%).
grid_randomly_flip() {
    permil=$1
    if [[ -z "$permil" ]]; then
        permil=10
    fi
    # echo "Flipping with probability $permil / 1000." >> $LOG
    for ((r = 0; r < ROWS; ++r)); do
        for ((c = 0; c < COLS; ++c)); do
            rnd=$RANDOM
            # echo "RANDOM returned $rnd" >> $LOG
            if [[ $[ rnd % 1000 ] -lt $permil ]]; then
                grid_flip_cell $r $c
            fi
        done
    done
}

grid_cell_is_alive() {
    if [[ "`grid_abs_get $1 $2`" = "$DEAD" ]]; then
        return 1
    fi
    return 0
}

# Runs life on the grid. The dumb but simple way.
grid_run_life() {
    local new_grid=()
    local min=2
    local max=3
    local new_row=""
    local count=0
    for ((r = 0; r < ROWS; r++)); do
        new_row=""
        for ((c = 0; c < COLS; c++)); do
            count=0
            if grid_cell_is_alive $r $c; then
                min=2
            else
                min=3
            fi
            if grid_cell_is_alive $[r-1] $[c-1]; then count=$[count+1]; fi
            if grid_cell_is_alive $[r-1] $c    ; then count=$[count+1]; fi
            if grid_cell_is_alive $[r-1] $[c+1]; then count=$[count+1]; fi
            if grid_cell_is_alive $r     $[c-1]; then count=$[count+1]; fi
            if grid_cell_is_alive $r     $[c+1]; then count=$[count+1]; fi
            if grid_cell_is_alive $[r+1] $[c-1]; then count=$[count+1]; fi
            if grid_cell_is_alive $[r+1] $c    ; then count=$[count+1]; fi
            if grid_cell_is_alive $[r+1] $[c+1]; then count=$[count+1]; fi
            # echo count for $r $c is $count >> $LOG
            if [[ $min -le $count && $count -le $max ]]; then
                new_row="$new_row$LIVE"
            else
                new_row="$new_row$DEAD"
            fi
        done
        new_grid[$r]="$new_row"
    done
    grid=(${new_grid[@]})
}

grid_init
# grid_randomly_flip 300
# grid_print "randomize at 20%"

# grid_goto 5 10; grid_place_block
# grid_goto 3 15; grid_place_blinker h
# grid_goto 7 15; grid_place_blinker v
# grid_goto 1 2; grid_place_glider ne
# grid_goto 5 2; grid_place_glider sw
# grid_goto 5 5;  grid_place_spaceship r

grid_goto 8 20;  grid_place_glider_gun se

grid_clear_print "initialized"
# exit 0

its=2000
for ((i=1; i <= its; ++i)); do
    grid_run_life
    grid_clear_print "iteration $i/$its..."
    # sleep 1
done
