#!/bin/bash

if [ -z "$PATH" ]; then
    export PATH="$DR_DIR/bin"
else
    export PATH="$DR_DIR/bin:$PATH"
fi

for dir in \
   /opt/local/bin \
   "$DR_OS_DIR/$DR_OS_NAME/bin" \
    ; do
    if [ -d "$dir" ]; then
        export PATH="$dir:$PATH"
    fi
done
