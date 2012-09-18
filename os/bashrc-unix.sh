#!/bin/bash

# Bash settings.

export EDITOR=emacs
export CLICOLOR=1
export HISTCONTROL=ignoreboth

# For git, use these if you don't want to run git config.
# export GIT_AUTHOR_NAME="Doug Rogers"     # git config --global user.name   "Doug Rogers"
# export GIT_AUTHOR_EMAIL="dr@noname.com"  # git config --global user.email  "dr@noname.com"
# export GIT_EDITOR="emacs"                # git config --global user.editor "emacs"

alias svnst="svn status -q "

svnstn()
{
    svn status $* | grep "^\\?"
}

# alias grepwd="cat ~/dr/trunk/amusements/itasoftware-puzzles/word.lst | grep "

source "$HOME/dr/os/unix/bash_functions.sh"
source "$HOME/dr/os/unix/prompt.sh"
