#!/bin/bash

# Bash settings.

export EDITOR=emacs
export CLICOLOR=1
export HISTCONTROL=ignoreboth

# # This is to merge history from all running shells:
# export HISTSIZE=$[1024 * 1024]
# export HISTFILESIZE=$[16 * 1024 * 1024]

# #LAST_HISTORY_WRITE=$SECONDS
# prompt_command() {
# #    if [ $(($SECONDS - $LAST_HISTORY_WRITE)) -gt 60 ]; then
#         history -a && history -c && history -r
# #        LAST_HISTORY_WRITE=$SECONDS
# #    fi
# }

PROMPT_COMMAND="prompt_command"

# For git, use these if you don't want to run git config.
# export GIT_AUTHOR_NAME="Doug Rogers"     # git config --global user.name   "Doug Rogers"
# export GIT_AUTHOR_EMAIL="dr@noname.com"  # git config --global user.email  "dr@noname.com"
# export GIT_EDITOR="emacs"                # git config --global user.editor "emacs"

alias svnst="svn status -q "

svnstn()
{
    svn status $* | grep "^\\?"
}

if [ -z "$HOME" ]; then
    if [ ! -z "$USERNAME" ]; then
        HOME="/c/Users/$USERNAME"
    fi
fi

export DR_DIR="$HOME/dr"
export PATH="$PATH:$DR_DIR/bin"

export DR_OS_DIR="$DR_DIR/os"

source "$DR_OS_DIR/bash-functions.sh"
source "$DR_OS_DIR/bash-aliases.sh"
source "$DR_OS_DIR/bash-prompt.sh"

# Now load the OS-specific bashrc.

export DR_OS_NAME=`uname -s | cut -d _ -f 1 | cut -d - -f 1 | tr A-Z a-z`

os_bashrc="$DR_OS_DIR/$DR_OS_NAME/bashrc.sh"

if [ -x "$os_bashrc" ]; then
    source "$os_bashrc"
fi

# Now load the site-specific bashrc.

site_file="$HOME/.dr/site"

if [ -f "$site_file" ]; then
    export DR_SITE="`cat \"$site_file\"`"
fi

site_bashrc="$HOME/dr/os/bashrc-${DR_SITE}.sh"

if [ -f "$site_bashrc" ]; then
    source "$site_bashrc"
fi
