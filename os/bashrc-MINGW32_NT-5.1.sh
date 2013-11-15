#!/bin/bash

alias ddk="cd ~/mandiant/src/sfAgent/agentModuleSDKs/AuditModules/mktools"

# Already in PATH from system.
# export PATH="$PATH:/c/Program Files (x86)/Microsoft Visual Studio 9.0/VC/bin/amd64"

emacs_bin_dir="/c/emacs/bin"
python_bin_dir="/c/Python27"

if [ -d "$emacs_bin_dir" ]; then
    export PATH="$PATH:$emacs_bin_dir:$python_bin_dir"
    export EDITOR=emacs
else
    export EDITOR=vi
fi
