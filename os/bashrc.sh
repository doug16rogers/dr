#!/bin/bash

export DR_OS=`uname -s`

os_file="$HOME/dr/os/$DR_OS/bashrc.sh"

if [ -x "$os_file" ]; then
    source "$os_file"
fi
