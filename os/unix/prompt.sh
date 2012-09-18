#!/bin/bash

# Sets the prompt for Unix systems.

source "$HOME/dr/os/unix/colors.sh"

color_user="$color_yellow"
color_host="$color_red"
color_dir="$color_CYAN"

if [ "`whoami`" == "root" ]; then
    prompt_char="#"
else
    prompt_char="$"
fi

export PS1="\n$color_user\u$color_reset@$color_host\h$color_reset $color_dir\w$color_reset\n$prompt_char "
