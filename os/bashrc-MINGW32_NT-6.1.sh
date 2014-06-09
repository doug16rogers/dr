#!/bin/bash

# Python.
python_bin_dir="/c/Python27"
export PATH="$PATH:$python_bin_dir"

# Emacs.
emacs_bin_dir="/c/emacs/bin"

if [ ! -d "$emacs_bin_dir" ]; then
    emacs_bin_dir="/c/tools/emacs/bin"  # Changed my standard place for this.
fi

if [ -d "$emacs_bin_dir" ]; then
    export PATH="$PATH:$emacs_bin_dir"
    export EDITOR=emacs
else
    export EDITOR=vi
fi
