#!/bin/bash

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
#include <stdio.h>
unsigned char data[0x10000];
unsigned char* dp = data;

int main(int argc, char* argv[]) {
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
