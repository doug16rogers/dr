#!/bin/bash

SCRIPT="$(basename $0)"
VERBOSE=0
TIMESTAMP=$(date '+%Y%m%d-%H%M%S')

usage() {
    echo ""
    echo "NAME"
    echo "    $SCRIPT - download server cert from an https site"
    echo ""
    echo "SYNOPSIS"
    echo "    $SCRIPT [OPTIONS] <domain-name>..."
    echo ""
    echo "DESCRIPTION"
    echo "    $SCRIPT downloads the server cert and stores it in a file"
    echo "    named '<domain-name>-YYYYMMDD-HHMMSS.crt'."
    echo ""
    echo "    -h          Print this usage information."
    echo "    -v          Print verbose information (dump certificate text)."
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
        *)
            log_error "invalid option '$opt'; use '-h' for help"
    esac
    shift
done

if [[ $# -eq 0 ]]; then
    echo "$SCRIPT: use '-h' for usage"
    exit 1
fi

while [ $# -gt 0 ]; do
    site="$1"
    shift
    if [[ "${site:0:8}" = "https://" ]]; then
        site="${site:8:${#site}}"
    fi
    site="$(echo $site | grep -o '^[^/]*')"
    certfile="${site}-${TIMESTAMP}.crt"
    last_certfile="$certfile"
    run "echo \
           | openssl s_client -servername '${site}' -connect '${site}:443' \
           2> /dev/null \
           | sed -ne '/-BEGIN CERTIFICATE-/,/-END CERTIFICATE-/p' \
           > '${certfile}'"
    echo "Downloaded certificate to '$certfile'."
    if [[ $VERBOSE -eq 1 ]]; then
        run "openssl x509 -in '$certfile' -noout -text"
    fi
done

if [[ $VERBOSE -eq 0 ]]; then
    echo
    echo "To view contents of the certificates, use '-v' or run:"
    echo
    echo "  $ openssl x509 -in '$last_certfile' -noout -text"
    echo
fi
