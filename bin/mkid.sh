#!/bin/bash

SCRIPT="$(basename $0)"
VERBOSE=0
CHARS=8
ENCODINGS=(base64 url uuid UUID hex HEX dec oct)
ENCODING_DESCRIPTIONS=(
    "'base64' uses 64 characters: 'A..Za..z0..9+/'."
    "'url' is base64 but with URL-safe characters: 'A..Za..z0..9-_'."
    "'uuid' creates a UUID of the form xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx"
    "'UUID' creates a UUID of the form XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX"
    "'hex' uses lower case hexadecimal characters: '0-9a-f'"
    "'HEX' uses upper case hexadecimal characters: '0-9A-F'"
    "'dec' uses decimal characters: '0-9'"
    "'oct' uses octal characters: '0-7'"
)
ENCODING="url"
DEVICE="/dev/urandom"

usage() {
    echo ""
    echo "NAME"
    echo "    $SCRIPT - make an ID suitable for use in various situations"
    echo ""
    echo "SYNOPSIS"
    echo "    $SCRIPT [OPTIONS]"
    echo ""
    echo "DESCRIPTION"
    echo "    $SCRIPT uses '/dev/urandom' to generate a random identifier."
    echo "    The resulting identifier is guaranteed to be random (as well as the random"
    echo "    device supports) across all its characters."
    echo ""
    echo "    -h          Print this usage information."
    echo "    -v          Print verbose messages (not many) to stderr."
    echo "    -c CHARS    Number of characters in output. [$CHARS]"
    echo "    -d DEVICE   Device for random data (use '-' to read from stdin). [$DEVICE]"
    echo "    -e ENCODING Output encoding. [$ENCODING]"
    echo ""
    echo "    Currently supported encodings:"
    for ((i=0; i < ${#ENCODINGS[@]}; i++)); do
        printf "        %-8s %s\n" "${ENCODINGS[$i]}" "${ENCODING_DESCRIPTIONS[$i]}"
    done
    echo ""
    exit $1
}

log_verbose() {
    if [ $VERBOSE -eq 1 ]; then
        echo "$SCRIPT: $@" 1>&2
    fi
}

log_error() {
    echo "$SCRIPT: $@" 1>&2
    exit 1
}

check_count() {
    if echo "$1" | grep "[^0-9]" >/dev/null 2>&1; then
        log_error "invalid character count (decimal number) '$1'"
    fi
    if [ $1 -lt 1 ]; then
        log_error "character too low in option '$1'"
    fi
}

check_device() {
    if [ "$1" = "-" ]; then
        return
    fi
    if [ -e "$1" ]; then
        return
    fi
    log_error "random source '$1' does not exist and is not '-'."
}

check_encoding() {
    for enc in ${ENCODINGS[@]}; do
        if [ "$enc" = "$1" ]; then
            return
        fi
    done
    log_error "invalid encoding '$1'"
}

while getopts "hvc:d:e:" opt; do
    case $opt in
        h)
            usage 0
            ;;
        v)
            VERBOSE=1
            ;;
        c)
            check_count "$OPTARG"
            CHARS="$OPTARG"
            ;;
        d)
            check_device "$OPTARG"
            DEVICE="$OPTARG"
            ;;
        e)
            check_encoding "$OPTARG"
            ENCODING="$OPTARG"
            ;;
        *)
            log_error "invalid option '$opt'; use '-h' for help"
    esac
done

DD_IF="if=$DEVICE"
if [ "$DEVICE" = "-" ]; then
   DD_IF=""
fi

for prog in base64 dd head od sed tr ; do
    if ! which $prog > /dev/null 2>&1; then
        log_error "'$prog' not found in path"
    fi
done

# $1 is "multiple of".
# $2 is the number to be rounded.
round_up_to_multiple_of() {
    n=$1
    x=$2
    echo $[x + (n - (x % n)) % n]
}

# Prints a random byte in decimal.
get_random_decimal_byte() {
    echo $(dd $DD_IF bs=1 count=1 2>/dev/null | od -t u1 -v -An | tr -d ' \n')
}

# BASE=$1 is the base for random digit.
# Returns a decimal number from 0 to BASE-1.
# A random byte read from the random source. If the byte is greater than the
# maximum multiple of BASE less than or equal to 256, then it is discarded
# and a new random byte is generated. This ensures that the modular operation
# will yield a proper random subset of the random byte.
get_random_in_base() {
    BASE=$1
    MAX_LEGIT=$[256 - (BASE % 256)]
    while [ -t ]; do
        BYTE=$(get_random_decimal_byte)
        if [ $BYTE -lt $MAX_LEGIT ]; then
            echo $[ BYTE % BASE ]
            break
        fi
    done
}

generate_base64() {
    m4=$(round_up_to_multiple_of 4 $CHARS)
    inchars=$[m4 * 3 / 4]
    id=$(dd $DD_IF bs=$inchars count=1 2>/dev/null | base64 | tr -d '\n')
    echo "${id:0:$CHARS}"
}

generate_url() {
    id=$(generate_base64 | sed -e 's/+/-/g' -e 's:/:_:g')
    echo "$id"
}

generate_uuid() {
    inchars=16
    id=$(dd $DD_IF bs=$inchars count=1 2>/dev/null | od -t x1 -v -An | head -1 | tr -d ' ' | sed -e 's/+/-/g' -e 's:/:_:g')
    id=$(echo "$id" | sed -e 's/\(........\)\(....\)\(....\)\(....\)\(............\)/\1-\2-\3-\4-\5/')
    echo "$id"
}

generate_UUID() {
    id=$(generate_uuid | tr '[:lower:]' '[:upper:]')
    echo "$id"
}

generate_hex() {
    m2=$(round_up_to_multiple_of 2 $CHARS)
    inchars=$[m2 / 2]
    id=$(dd $DD_IF bs=$inchars count=1 2>/dev/null | od -t x1 -v -An | tr -d ' ')
    echo "${id:0:$CHARS}"
}

generate_HEX() {
    id=$(generate_hex | tr '[:lower:]' '[:upper:]')
    echo "$id"
}

generate_id_in_base() {
    base=$1
    id=""
    while [ ${#id} -lt $CHARS ]; do
        digit=$(get_random_in_base $base)
        id="$id$digit"
    done
    echo "$id"
}

generate_oct() {
    generate_id_in_base 8
}

generate_dec() {
    generate_id_in_base 10
}

generate_${ENCODING}

if [ "$DEVICE" = "-" ]; then
    # Read line from stdin to avoid data spewing to any parent shell.
    read line
    # Close stdin for our running subprocess.
    0<&-
fi
