#!/bin/bash

# Copyright (c) 2016-2019 Doug Rogers under the Zero Clause BSD License.
# You are free to do whatever you want with this software. See LICENSE.txt.

# Compiles a brainfuck program stored in a file.

if [ $# -lt 2 ]; then
    if [ "$1" == "--version" ]; then
        echo "heh. funny. a version."
        exit 0
    else
        echo "Usage: bf.sh <output-program> <branfuck-source-file>..."
        exit 1
    fi
fi

exe="$1"
shift
c_file="$exe.bf.c"

cat <<EOF > "$c_file"
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
unsigned char data[0x20000];
unsigned char* dp = &data[sizeof(data)/2];;

void sig_handler(int sig) {
    exit(1);
}

int main(int argc, char* argv[]) {
    signal(SIGSEGV, sig_handler);
EOF

while [ $# -gt 0 ]; do
    file="$1"
    shift
    cat "$file" | sed -e 's/[^-+,.\[\]<>]//g' | \
        sed -e 's/-/(*dp)--;N/g' \
            -e 's/+/(*dp)++;N/g' \
            -e 's/,/*dp=getchar();N/g' \
            -e 's/\./putchar(*dp);N/g' \
            -e 's/</dp--;N/g' \
            -e 's/>/dp++;N/g' \
            -e 's/\[/while(*dp) {N/g' \
            -e 's/]/}N/g' \
            | tr N '\n' >> "$c_file"
done

cat <<EOF >> "$c_file"
   return 0;
}
EOF

gcc -o "$exe" "$c_file"
