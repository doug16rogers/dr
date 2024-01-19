#!/bin/bash

# Sets some helpful aliases.

alias tc="transmission-cli 2> /dev/null "

# alias ls="ls --color=auto "    # Doesn't work in MacOS.
alias grepwd="cat $HOME/dr/fun/itasoftware-puzzles/word.lst | grep "

# To fully encrypt a 7zip archive, including the headers: 7ze <archive> <dir/files>...
alias 7ze="7z a -r -p -mhe=on "

# Shredding should include zeroing at the end. Note that this probably
# doesn't matter on a flash drive since I don't think it actually writes 0s
# in place (though it *could*!).
alias shredusb="shred -n0 -z --remove "

# Open any file using its default application.
alias launch="xdg-open 2> /dev/null "

# cd to the directory in ~/.cur - current project I'm working on.
alias cdcur='cd $(cat ~/.cur)'

# Get rid of the annoying \$ effect when running tmux.
alias completedir='complete -r; shopt -s direxpand'

# Make ffmpeg/ffprobe easier to read.
alias ffm='ffmpeg -hide_banner -loglevel warning -stats'
alias ffM='ffmpeg -hide_banner'
alias ffi="ffprobe -hide_banner -show_entries 'stream_tags : format_tags : program_stream_tags' "

ffa() {
    if [ $# -ne 2 ]; then
        echo "Usage: ffa <infile> <outfile>"
        exit 1
    fi
    ffm -i "$1" -c copy "$2"
    append-vid-specs.py "$2"
}

# A lot of git aliases...
alias g1='git log --format="%ci %Cgreen%h%Creset %Cred(%cn)%Creset %s" --date=local --abbrev-commit'
alias g1m='g1 master...'
alias glog='git log --name-status '  # Show files changed with each commit.
alias gb='git branch '               # List all local branches.
alias gb1='git branch | grep "^[*]"' # List currently checked out branch.
alias gba='git branch -a'            # List all branches, including remotely tracked ones.
alias gc="git checkout "
alias gd='git diff '                 # Diff since the named commit.
alias gdno='gd --name-only '         # Only the files change over the range of commits.
alias gi="git branch | grep '^[*]'; git describe; git remote -v | grep -v push; g1 -3 | cat"
alias gs='git status '
alias gf='git show --pretty="format:" --name-only '   # Show files for a commit.
alias gbd='git branch -d '
alias grc='git rebase --continue '
alias gmt='git mergetool -y '
alias mv='mv -i '
alias startsshagent='eval $(ssh-agent | tee "$HOME/ssh-agent.out")'
alias vlcc='vlc >/dev/null 2>&1 '
alias vlcp='vlc --play-and-exit >/dev/null 2>&1 '

if [ $DR_OS_NAME == "darwin" ]; then
    alias vlc='/Applications/VLC.app/Contents/MacOS/VLC'
else
    alias pbcopy='xclip -selection clipboard -i'
    alias pbpaste='xclip -selection clipboard -o'
fi

alias vlcc='vlc >/dev/null 2>&1'
alias vlcp='vlc --play-and-exit >/dev/null 2>&1'

gd1() {         # Show changes just in the named commits, one at a time.
    while [[ $# -gt 0 ]]; do
        gd $1{^,}
        shift
    done
}

gdno1() {       # Show files changed in the named commits, one at a time.
    while [[ $# -gt 0 ]]; do
        gdno $1{^,}
        shift
    done
}

gbr() {         # Shows remote branches.
    run "git ls-remote $* | grep heads/ | sed -e 's:^.*heads/::'"
}

gbdr() {        # Deletes remote branch: gbdr <remote-repo> <remote-branch>
    del_opt="-d"
    if [[ "$1" == "-D" ]]; then
        del_opt="-D"
        shift
    fi

    if [[ -z "$2" ]]; then
        echo "Usage: gbdr [-D] remote-name branch..."
        echo "    Also deletes local tracking branch. Use '-D' to delete unmerged branches."
        echo "    Examples:"
        echo "         gbdr    dr prep-v5.11.0"
        echo "         gbdr -D dr fix-that-did-not-work"
    else
        remote="$1"
        shift
        while [[ $# -gt 0 ]]; do
            branch="$1"
            shift
            run "git push --delete '$remote' '$branch'"      # Delete branch in external repo.
            run "git branch $del_opt -r '$remote/$branch'"   # Delete tracking branch if it exists.
            run "git branch $del_opt '$branch'"              # Delete local branch.
        done
    fi
}

# Subversion shortcuts:

alias svnst="svn status -q "

svnstn() {
    run "svn status $* | grep '^[?]'"
}
