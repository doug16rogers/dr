#!/bin/bash

wget -q -O - \
     -U 'Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:47.0) Gecko/20100101 Firefox/47.0' \
     http://whatismyipaddress.com \
| grep -i 'click for more about' \
| sed -e 's/^.*lick for more about //' -e 's/".*$//'
