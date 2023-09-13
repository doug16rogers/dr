#!/bin/bash

# Sets the prompt for Unix systems.

source "$DR_OS_DIR/bash-colors.sh"

color_time="$color_gray"
color_user="$color_yellow"
color_host="$color_red"
color_dir="$color_CYAN"
color_hist="$color_reset"
color_char="$color_reset"
color_git="$color_magenta"

git_branch_text=""

# Download from https://github.com/git/git/blob/master/contrib/completion/git-prompt.sh
if [[ -e "$DR_OS_DIR/git-prompt.sh" ]]; then
    source "$DR_OS_DIR/git-prompt.sh"
    git_branch_text=" \$(__git_ps1 '$color_git(%s)$color_reset')"
fi

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
    prompt_date='`hexdate -n --epoch=doug; date -u "+ %y%m%d-%H%M%S"`'
else
    prompt_date='`date -u "+%y%m%d-%H%M%S"`'
fi

prompt_hist="\`history 1 | awk '{print \$1}' | (read _hist && echo \$[_hist+1])\`"

# Readline (on MacOS anyway) struggles with complicated prompts, especially
# when there are escape sequences embedded in the prompt after the last
# newline. So just have the prompt character + space after the last newline.
export PS1="\n$color_hist$prompt_hist $color_time${prompt_date} $color_user\u$color_reset@$color_host\h$color_reset$git_branch_text $color_dir\w$color_reset\n$prompt_char "

# echo "PS1=\"$PS1\""
