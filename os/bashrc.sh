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

# PROMPT_COMMAND="prompt_command"

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
export DR_BIN="$DR_DIR/bin"
export DR_OS_DIR="$DR_DIR/os"

# This needs to be set before bash-path.sh.
export DR_OS_NAME=`uname -s | cut -d _ -f 1 | cut -d - -f 1 | tr A-Z a-z`

if [ ${DR_OS_NAME:0:5} == "mingw" ]; then
    export DR_OS_NAME="mingw"
fi

for subbash in \
        path \
        functions \
        aliases \
        prompt \
        fzf \
        pyenv \
        tab-completion;
do
    subfile="$DR_OS_DIR/bash-${subbash}.sh"
    if [[ -e "$subfile" ]]; then
        source "$subfile"
    fi
done

# Other specific packages:

if [[ -e "$HOME/.cargo/env" ]]; then
    source "$HOME/.cargo/env"
fi

# Load the OS-specific bashrc.

os_bashrc="$DR_OS_DIR/$DR_OS_NAME/bashrc.sh"

if [[ -x "$os_bashrc" ]]; then
    source "$os_bashrc"
fi

# Load the site-specific bashrc.

site_file="$HOME/.dr/site"

if [[ -f "$site_file" ]]; then
    export DR_SITE="`cat \"$site_file\"`"
fi

site_bashrc="$HOME/dr/os/bashrc-${DR_SITE}.sh"

if [[ -f "$site_bashrc" ]]; then
    source "$site_bashrc"
fi

ssh_agent_file="$HOME/ssh-agent.out"

if [[ -e "$ssh_agent_file" ]]; then
    # Seems more secure than `source "$ssh_agent_file"`:
    ssh_auth_sock=$(cat "$ssh_agent_file" | grep '^SSH_AUTH_SOCK' | sed -e 's/SSH_AUTH_SOCK=\([-+_./0-9a-zA-Z]*\);.*$/\1/')
    ssh_agent_pid=$(cat "$ssh_agent_file" | grep '^SSH_AGENT_PID' | sed -e 's/SSH_AGENT_PID=\([0-9]*\);.*$/\1/')
    export SSH_AUTH_SOCK="$ssh_auth_sock"
    export SSH_AGENT_PID="$ssh_agent_pid"
fi

brew_file="/home/linuxbrew/.linuxbrew/bin/brew"

if [[ -x "$brew_file" ]]; then
    eval "$($brew_file shellenv)"
fi

if [[ -n "$(which direnv)" ]]; then
    eval "$(direnv hook bash)"
fi
