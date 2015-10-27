#!/bin/bash

# Sets some helpful aliases.

# alias ls="ls --color=auto "    # Doesn't work in MacOS.
alias grepwd="cat $HOME/dr/fun/itasoftware-puzzles/word.lst | grep "

# A lot of git aliases...
alias g1='git log --format="%ci %Cgreen%h%Creset %ct %Cred(%cn)%Creset %s" --date=local --abbrev-commit'
alias glog='git log --name-status '  # Show files changed with each commit.
alias gb='git branch -a '            # List all branches, including remotely tracked ones.
alias gc="git checkout "
alias gd='git diff '
alias gs='git status '
alias gf='git show --pretty="format:" --name-only '   # Show files for a commit.
alias gbd='git branch -d '

gbr()           # Shows remote branches.
{
    run "git ls-remote $* | grep heads/ | sed -e 's:^.*heads/::'"
}

gbdr()          # Deletes remote branch: gbdr <remote-repo> <remote-branch>
{
    if [ -z "$2" ]; then
        echo "Usage: gbdr remote-name remote-branch [-D]"
        echo "       Also deletes local tracking branch."
    else
        del_opt="-d"
        if [ "$3" == "-D" ]; then
            del_opt="-D"
        fi
        run "git push --delete '$1' '$2'"       # Delete branch in external repo.
        run "git branch $del_opt -r '$1/$2'"    # Delete tracking branch if it exists.
        run "git branch $del_opt '$2'"          # Delete local branch.
    fi
}

# Subversion shortcuts:

alias svnst="svn status -q "

svnstn()
{
    run "svn status $* | grep '^[?]'"
}
