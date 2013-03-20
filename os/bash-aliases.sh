#!/bin/bash

# Sets some helpful aliases.

alias grepwd="cat $HOME/dr/fun/itasoftware-puzzles/word.lst | grep "

alias glog="git log --name-status "
alias gb="git branch -a "
alias gd="git diff "
alias gs="git status "

alias svnst="svn status -q "

svnstn()
{
    svn status $* | grep "^\\?"
}

