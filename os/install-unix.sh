#!/bin/bash
# Copyright (c) 2012 Doug Rogers under the terms of the MIT License.
# See http://www.opensource.org/licenses/mit-license.html..
# $Id$

if [ -z "$1" ]; then
    echo "Usage: install-unix.sh <site-name>"
    echo
    echo "Creates ~/.dr/site and installs .bashrc, .emacs, .screenrc, etc. to \$HOME."
    echo
    exit 1
fi

run()
{
    cmd="$*"
    echo "$cmd"
    eval "$cmd"
}


site="$1"
save="~/.dr/save"

run mkdir -p "$save"
run "echo $site > ~/.dr/site"

run mv ~/.bashrc "$save"
run cp ~/dr/os/bashrc-main.sh ~/.bashrc

run mv ~/.screenrc "$save"
run cp ~/dr/os/screenrc-main ~/.screenrc

run mv ~/.emacs "$save"
run cp ~/dr/os/emacs-main.el ~/.emacs

if [ "`uname -s`" == "Darwin" ]; then
    run cp ~/.profile "$save"
    run "cat ~/dr/os/profile-main.sh >> ~/.profile"
fi
