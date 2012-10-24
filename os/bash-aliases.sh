#!/bin/bash

# Sets some helpful aliases.

alias grepwd="cat $HOME/dr/fun/itasoftware-puzzles/word.lst | grep "

alias svnst="svn status -q "

svnstn()
{
    svn status $* | grep "^\\?"
}

