#!/bin/sh

# Taken from: https://help.github.com/articles/changing-author-info/

if [ -z "$3" ]; then
    echo "Usage: git-author-rewrite.sh <old-email> \"<new-name>\" <new-email>"
    echo ""
    echo "  This should be used on a bare repo then pushed back. Use something like this:"
    echo ""
    echo "    $ git clone --bare https://github.com/organization/repo.git repo.git"
    echo ""
    echo "  or, for a local repo:"
    echo ""
    echo "    $ git clone --bare file:///path/to/repo/.git repo.git"
    echo ""
    echo "  then"
    echo ""
    echo "    $ cd repo.git"
    echo "    $ git-author-rewrite.sh mickey@disney.com 'Olive Oyl' ooyl@universal.com"
    echo "    # git log -10    # verify changes in history"
    echo "    $ git push --force --tags origin 'refs/heads/*'"
    echo ""
    echo "  From: https://help.github.com/articles/changing-author-info/"
    echo ""
    exit 1
fi

arg1="$1"
arg2="$2"
arg3="$3"
shift 3

echo "Replacing email '$arg1' with user name '$arg2' and email '$arg3'."

if [ ! -z "$1" ]; then
    echo "Extra git options: $*"
fi

git filter-branch $* --env-filter "

OLD_EMAIL='$arg1'
CORRECT_NAME='$arg2'
CORRECT_EMAIL='$arg3'

if [ \"\$GIT_COMMITTER_EMAIL\" = \"\$OLD_EMAIL\" ]
then
    export GIT_COMMITTER_NAME=\"\$CORRECT_NAME\"
    export GIT_COMMITTER_EMAIL=\"\$CORRECT_EMAIL\"
fi
if [ \"\$GIT_AUTHOR_EMAIL\" = \"\$OLD_EMAIL\" ]
then
    export GIT_AUTHOR_NAME=\"\$CORRECT_NAME\"
    export GIT_AUTHOR_EMAIL=\"\$CORRECT_EMAIL\"
fi
" --tag-name-filter cat -- --branches --tags
