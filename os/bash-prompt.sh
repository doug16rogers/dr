#!/bin/bash

# Sets the prompt for Unix systems.

source "$DR_OS_DIR/bash-colors.sh"

color_time="$color_gray"
color_user="$color_yellow"
color_host="$color_red"
color_dir="$color_CYAN"

prompt_char="$"

user="`whoami`"

if [ $? -eq 0 -a "$user" == "root" ]; then
    prompt_char="#"
fi

export PS1="\n$color_time\$(date -u +%y%m%d-%H%M%S) $color_user\u$color_reset@$color_host\h$color_reset $color_dir\w$color_reset\n$prompt_char "
