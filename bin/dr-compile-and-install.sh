#!/bin/bash

fundir=$DR_DIR/fun

sudo apt install libcapstone-dev        # for csdisasm
sudo apt install libgmp-dev             # for radixify

for fun in bintools charts csdisasm hex hexdate radixify unicode/echo-utf8; do
    echo "make -C '$fundir/$fun' install"
    make -C $fundir/$fun install
done