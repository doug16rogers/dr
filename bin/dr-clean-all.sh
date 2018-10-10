#!/bin/bash

if [[ -z "$DR_DIR" ]]; then
    export DR_DIR="$HOME/dr"
fi

if [[ ! -d "$DR_DIR" ]]; then
    echo "No DR_DIR='$DR_DIR'."
    exit 1
fi

echo "find \"$DR_DIR\" -type f -name Makefile | (while read path; do make -C \`dirname \"\$path\"\` clean; done)"
find "$DR_DIR" -type f -name Makefile | (while read path; do make -C `dirname "$path"` clean; done)

echo "find \"$DR_DIR\" -type d -name '*.dSYM' Makefile -exec rm -rf {} \; 2>/dev/null"
find "$DR_DIR" -type d -name '*.dSYM' Makefile -exec rm -rf {} \; 2>/dev/null
