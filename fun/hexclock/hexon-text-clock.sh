#! /bin/bash

# Copyright (c) 2012-2019 Doug Rogers under the Zero Clause BSD License.
# You are free to do whatever you want with this software. See LICENSE.txt.

while [ -t ] ; do
  echo -e -n "\r`hexdate`"
done
