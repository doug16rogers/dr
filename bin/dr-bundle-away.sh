#!/usr/bin/env bash

SCRIPT=$(basename $0)

TIMESTAMP=$(date '+%Y%m%d-%H%M%S')

zip_file="bundle-$TIMESTAMP.zip"
enc_file="$zip_file.gpg"

usage() {
    echo ""
    echo "$SCRIPT"
    echo ""
    echo "This script will push a set of files from here to somewhere else using"
    echo "scp. Multiple destinations may be specified on the command line."
    echo ""
    echo "Destinations are scp paths, including the colon ':'. So 'user@host:path/'"
    echo "or a name that appears in a 'Host' line in '~/.ssh/config', like 'home:'."
    echo ""
    echo "The list of files to read is found in '~/.bundle/files.txt'."
    echo "The list of gpg recipients to encrypt to is in '~/.bundle/recipients.txt'."
    echo ""
    echo "USAGE"
    echo ""
    echo "  \$ $SCRIPT [OPTIONS] <ssh-host-path>..."
    echo ""
    echo "OPTIONS"
    echo ""
    echo "Currently there are no options. Intermediate filenames hard-coded here."
    echo ""
}

cleanup() {
    echo "rm -f '$zip_file' '$enc_file'"
    rm -f "$zip_file" "$enc_file"
}

fail_with() {
    echo "$SCRIPT: fail"
    exit $1
}

function signal_handler() {
    echo ""
    echo "$SCRIPT: signal captured (Ctrl+C likely)."
    cleanup
    stty sane
    exit 21
}

if [[ $# -eq 0 ]]; then
    echo ""
    echo "$SCRIPT: no arguments (destinations) specified."
    usage
    exit 1
fi

echo "cd '$HOME'"
cd "$HOME"

files_file="$HOME/.bundle/files.txt"

if [[ ! -e "$files_file" ]]; then
    echo "$SCRIPT: file list '$files_file' does not exist."
    exit 1
fi

recipients_file="$HOME/.bundle/recipients.txt"

if [[ ! -e "$recipients_file" ]]; then
    echo "$SCRIPT: gpg recipients list '$recipients_file' does not exist."
    exit 1
fi

recipients_arg=''
while read recipient; do
    if ! gpg -k "$recipient" >/dev/null 2>&1; then
        echo "$SCRIPT: recipient '$recipient' not in gpg keychain."
        fail_with 2
    fi
    echo "Adding recipient '$recipient'."
    recipients_arg="$recipients_arg -r $recipient"
done < "$recipients_file"

# Clean up files if Ctrl+C is pressed.
# trap signal_handler SIGINT

echo "cat '$files_file' | zip -@ -j '$zip_file'"
cat "$files_file" | zip -@ -j "$zip_file"

if [[ $? -ne 0 ]]; then
    echo "$SCRIPT: zip failed"
    cleanup
    fail_with 3
fi

echo "gpg --encrypt --output '$enc_file' $recipients_arg '$zip_file'"
gpg --encrypt --output "$enc_file" $recipients_arg "$zip_file"

if [[ $? -ne 0 ]]; then
    echo "$SCRIPT: gpg encryption failed"
    cleanup
    fail_with 4
fi

while [[ $# -gt 0 ]]; do
    dest="$1"
    shift

    if [[ ! "$dest" == *":"* ]]; then
        echo "$SCRIPT: no ':' in destination '$dest' (not valid scp remote destination)"
        cleanup
        fail_with 5
    fi

    echo "scp $enc_file $dest"
    scp "$enc_file" "$dest"

    if [[ $? -ne 0 ]]; then
        echo "$SCRIPT: gpg encryption failed"
        cleanup
        fail_with 6
    fi
done

cleanup
echo ok
exit 0
