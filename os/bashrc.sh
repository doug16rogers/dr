#!/bin/bash

export dr_OS=`uname -s`

os_file="$HOME/dr/os/$dr_OS/bin/bashrc.sh"

if [ -x "$os_file" ]; then
    source "$os_file"
fi
