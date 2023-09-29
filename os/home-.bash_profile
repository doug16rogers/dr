#!/bin/bash

# Load .bashrc if this is an interactive bash shell.
if [ -n "$BASH_VERSION" -a -n "$PS1" ]; then
    if [ -e "$HOME/.bashrc" ]; then
        source "$HOME/.bashrc"
    fi
fi
