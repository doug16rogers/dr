#!/bin/bash

export PATH="$HOME/dr/os/$dr_OS/bin:$PATH"
export EDITOR=emacs
export CLICOLOR=1      # This is now in /etc/profile.
export HISTCONTROL=ignoreboth

# See http://en.wikipedia.org/wiki/ANSI_escape_code
csi="\033["
lo="0;"
hi="1;"
fg=3
bg=4
black=0
red=1
green=2
yellow=3
blue=4
magenta=5
cyan=6
white=7
end=m
color_reset="${csi}0$end"
color_user="$csi$lo$fg$yellow$end"
color_host="$csi$lo$fg$red$end"
color_dir="$csi$hi$fg$cyan$end"

if [ "`whoami`" == "root" ]; then
    prompt_char="#"
else
    prompt_char="$"
fi

export PS1="\n$color_user\u$color_reset@$color_host\h$color_reset $color_dir\w$color_reset\n$prompt_char "
