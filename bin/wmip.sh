#!/bin/bash

SCRIPT="$(basename $0)"
VERBOSE=0
SHOW_LOCATION=0
SERVER="https://www.whatismypublicip.com"
USER_AGENT='Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:47.0) Gecko/20100101 Firefox/47.0'
TMPFILE="/tmp/ip-loc.txt"
KEEP_TMPFILE=0

usage() {
    echo ""
    echo "NAME"
    echo "    $SCRIPT - determine public IPv4 address"
    echo ""
    echo "SYNOPSIS"
    echo "    $SCRIPT [OPTIONS]"
    echo ""
    echo "DESCRIPTION"
    echo "    $SCRIPT uses wget to a web server that reports the caller's"
    echo "    public-facing IP address."
    echo ""
    echo "    -h          Print this usage information."
    echo "    -v          Print debug information."
    echo "    -l          Show location information."
    echo "    -k          Keep the temporary file, '$TMPFILE'."
    echo "    -S SERVER   Server to use for IP reporting service. [$SERVER]"
    echo "    -U USERAGT  User agent string."
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
        v)
            VERBOSE=1
            ;;
        l)
            SHOW_LOCATION=1
            ;;
        k)
            KEEP_TMPFILE=1
            ;;
        S)
            SERVER="$OPTARG"
            ;;
        U)
            USER_AGENT="$OPTARG"
            ;;
        *)
            log_error "invalid option '$opt'; use '-h' for help"
    esac
done

run "wget -q -O - -U '$USER_AGENT' '$SERVER' > $TMPFILE"

cmd="cat $TMPFILE | egrep -o '[0-9]+[.][0-9]+[.][0-9]+[.][0-9]+'"
log_verbose "$cmd"
ip=$(eval $cmd)

if [[ $SHOW_LOCATION -eq 1 ]]; then
    cmd="cat $TMPFILE | grep -A 1 City: | tail -1 | sed -e 's/^[^>]*>//' -e 's/<.*$//'"
    log_verbose $cmd
    city=$(eval $cmd)

    cmd="cat $TMPFILE | grep -A 1 Region: | tail -1 | sed -e 's/^[^>]*>//' -e 's/<.*$//'"
    log_verbose $cmd
    region=$(eval $cmd)

    cmd="cat $TMPFILE | grep -A 1 Country: | tail -1 | sed -e 's/^[^>]*>//' -e 's/<.*$//'"
    log_verbose $cmd
    country=$(eval $cmd)

    cmd="cat $TMPFILE | grep -A 1 Latitude: | tail -1 | sed -e 's/^[^>]*>//' -e 's/<.*$//'"
    log_verbose $cmd
    lat=$(eval $cmd)

    cmd="cat $TMPFILE | grep -A 1 Longitude: | tail -1 | sed -e 's/^[^>]*>//' -e 's/<.*$//'"
    log_verbose $cmd
    lon=$(eval $cmd)
fi

echo $ip
if [[ $SHOW_LOCATION -eq 1 ]]; then
    echo "$city, $region, $country"
    echo "$lat, $lon"
fi

if [[ $KEEP_TMPFILE -ne 1 ]]; then
    rm -f $TMPFILE
fi
