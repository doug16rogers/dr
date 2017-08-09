#!/bin/bash

script=`basename $0`

if [ ! -d .git ]; then
    echo "$script: must be in root of git repository (with .git subdir)" >2
    exit 1
fi

svn_username=`whoami`

if [ ! -z "$1" ]; then
    svn_username="$1"
    shift
fi

git_base=`pwd`
echo "git_base='$git_base'"

# Echo then run a command.
run()
{
    cmd="$*"

    if [ -z "$LOG_FILE" ] ; then
        echo "$cmd"
        eval "$cmd"
    else
        log "$cmd"
        set -o pipefail    # Bash 3.0 and greater. Google this for more info.
        eval "$cmd" 2>&1 | tee -a "$LOG_FILE"
    fi

    result=$?
    return $result
}

echo "# Listing externals..."
run mkdir -p .git_externals
if ! grep --quiet "^.git_externals$" .git/info/excludes; then
    run "echo .git_externals >> .git/info/excludes"
fi
if [ ! -e .git_externals.txt ]; then   # REMOVE ME
    run 'git svn show-externals | grep // | tee .git_externals.txt'
fi
url_base=$(git svn info --url | sed -e 's,^\([^/]*//[^/]*/\).*,\1,')
echo "url_base='$url_base'"

if [ ! -z "$svn_username" ]; then
    echo "# Using svn username '$svn_username'; adjusting url_base:"
    url_base=$(echo $url_base | sed -e "s,//,//$svn_username@,")
    echo "url_base='$url_base'"
fi

cat .git_externals.txt |
while read external ; do
    echo ""
    echo "# Handling external '$external'."
    loc_path=$(echo $external | sed -e 's,^/\(.*\)//.*$,\1,')
    echo "loc_path='$loc_path'"
    # @todo Handle spaces in directory names.
    url_path=$(echo $external | sed -e 's,^.*//\([^ ]*\) .*$,\1,')
    echo "url_path='$url_path'"
    lib_name=$(echo $external | sed -e 's,^.* \([^ ]*\)$,\1,')
    echo "lib_name='$lib_name'"
    ext_base=".git_externals/$loc_path"
    run "mkdir -p '$ext_base'"
    # @todo Should I use git-svn for these checkouts, too?
    lib_path="$loc_path/$lib_name"
    if [ -d "$git_base/$ext_base/$lib_name" ]; then
	echo "# '$git_base/$ext_base/$lib_name' already exists; not updating. Do it manually if necessary."
    else
	run "(cd '$ext_base' && svn checkout '${url_base}${url_path}' '$lib_name')"
    fi
    if [ -d "$lib_path" ]; then
	echo "# '$lib_path' already exists; skipping symlink creation."
    else
	run "ln -s '$git_base/$ext_base/$lib_name' '$lib_path'"
    fi
    if ! grep --quiet "^$lib_path$" .git/info/excludes; then
	run "echo '$lib_path' >> .git/info/excludes"
    fi
done
