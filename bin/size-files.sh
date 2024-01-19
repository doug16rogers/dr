#!/bin/bash

SCRIPT="$(basename $0)"
VERBOSE=0
BYTES=0

usage() {
    echo ""
    echo "NAME"
    echo "    $SCRIPT - determine total size of files and directories on command line."
    echo ""
    echo "SYNOPSIS"
    echo "    $SCRIPT [OPTIONS] [FILES...]"
    echo ""
    echo "DESCRIPTION"
    echo "    $SCRIPT uses 'stat' to get information about a file. By default the script"
    echo "    prints human-readable sizes (k, g, t, ...). Use '-b' to print in bytes."
    echo ""
    echo "    -h          Print this usage information."
    echo "    -b          Print size in bytes rather than human-readable."
    echo ""
    exit $1
}

log_verbose()
{
    if [[ $VERBOSE -eq 1 ]]; then
        echo "$*"
    fi
}

run()
{
    cmd="$*"
    log_verbose "$cmd"
    eval "$cmd"
}

while getopts "hvlkS:U:" opt; do
    case $opt in
        h)
            usage 0
            ;;
        b)
            BYTES=1
            ;;
        *)
            log_error "invalid option '$opt'; use '-h' for help"
            ;;
    esac
done

filesize() {
    f="$1"
    if [ -d "$f" ]; then
        size=$(du -cb "$f")
    else
        size=$(stat -c %s "$f")
    fi
    echo $size
}

total=0

if [ $# -eq 0 ]; then
    total=$(filesize ".")
else
    while [[ $# -gt 0 ]]; do
        f="$1"
        size=$(filesize "$f")
        total=$[total + size]
        shift
    done
fi

if [[ $BYTES -eq 1 ]]; then
    echo $total
    exit 0
fi

div=$[1024 * 1024 * 1024 * 1024]
if [[ $total -gt $div ]]; then
    printf "%u.%03uT\n" $[total / div] $[(total % div) * 1000 / div]
    exit 0
fi

div=$[1024 * 1024 * 1024]
if [[ $total -gt $div ]]; then
    printf "%u.%03uG\n" $[total / div] $[(total % div) * 1000 / div]
    exit 0
fi

div=$[1024 * 1024]
if [[ $total -gt $div ]]; then
    printf "%u.%03uM\n" $[total / div] $[(total % div) * 1000 / div]
    exit 0
fi

div=$[1024]
if [[ $total -gt $div ]]; then
    printf "%u.%03uK\n" $[total / div] $[(total % div) * 1000 / div]
    exit 0
fi

printf "%u\n" $total
