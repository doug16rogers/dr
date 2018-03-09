#!/bin/bash

if [[ -z "$DR_DIR" ]]; then
    export DR_DIR="$HOME/dr"
fi

if [[ -z "$DR_BIN" ]]; then
    export DR_BIN="$DR_DIR/bin"
fi

fundir="$DR_DIR/fun"

# Install packages needed to build apps:
#   csdisasm: libcapstone-dev
#   radixify: libgmp-dev

for pkg in libcapstone-dev libgmp-dev; do
    if (dpkg -l "$pkg" 2> /dev/null | grep "$pkg" > /dev/null 2>&1); then
        echo "'$pkg' already installed."
    else
        echo "Need to install package '$pkg'..."
        sudo apt install "$pkg"
    fi
done

# Build apps:

for app in bintools charts csdisasm hex hexdate radixify unicode/echo-utf8 wtools; do
    echo "make -C '$fundir/$app' install"
    make -C $fundir/$app install
done
