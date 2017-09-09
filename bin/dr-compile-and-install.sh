#!/bin/bash

fundir=$DR_DIR/fun

sudo apt install libcapstone-dev        # for csdisasm
sudo apt install libgmp-dev             # for radixify

for fun in bintools cdisasm hex hexdate radixify; do 
    echo "make -C '$fundir/$fun' install"
    make -C $fundir/$fun install
done
