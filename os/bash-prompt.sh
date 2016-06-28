#!/bin/bash

# Sets the prompt for Unix systems.

source "$DR_OS_DIR/bash-colors.sh"

color_time="$color_gray"
color_user="$color_yellow"
color_host="$color_red"
color_dir="$color_CYAN"

prompt_char="$"

is_root="0"

if [ "${OS:0:7}" == "Windows" ]; then
    # See http://stackoverflow.com/questions/4051883/batch-script-how-to-check-for-admin-rights/
    if fsutil dirty query "$SYSTEMDRIVE" > /dev/null 2>&1 ; then
        is_root="1"
        cmd //c title Admin
    fi
else
    user="`whoami`"
    if [ $? -eq 0 -a "$user" == "root" ]; then
        is_root="1"
    fi
fi

if [ $is_root -eq 1 ]; then
    prompt_char="#"
fi

if [ -x "$DR_DIR/bin/hexdate" ]; then
    prompt_date='$(hexdate -n --epoch=doug; date -u "+ %y%m%d-%H%M%S")'
else
    prompt_date='$(date -u "+%y%m%d-%H%M%S")'
fi

export PS1="\n$color_time${prompt_date} $color_user\u$color_reset@$color_host\h$color_reset $color_dir\w$color_reset\n$prompt_char "
