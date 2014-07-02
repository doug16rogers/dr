# Useful (ba)sh functions. 

# -----------------------------------------------------------------------------
# mkdir-chdir
function mcd()
{
    mkdir -p "$1"
    cd "$1"
}

# -----------------------------------------------------------------------------
# Puts the message (all arguments) to $LOG_FILE and to stdout.
# If set, uses PROC_NAME, LOG_ELAPSED_TIME, and LOG_TIME_FORMAT.
# See the code for details. It ain't that hard.

log()
{
    if [ -z "$LOG_FILE" ] ; then
        LOG_FILE=/tmp/build.log
    fi

    if [ -z "$PROC_NAME" ] ; then
        name=""
    else
        name=" $PROC_NAME"
    fi

    if [ "$LOG_ELAPSED_TIME" = "yes" ] ; then
        timestamp=`elapsed`
    elif [ ! -z "$LOG_TIME_FORMAT" ] ; then
        timestamp=`date "$LOG_TIME_FORMAT"`
    else
        LOG_TIME_FORMAT="+%Y-%m-%d %H:%M:%S"
        timestamp=`date "$LOG_TIME_FORMAT"`
    fi

    echo "${timestamp}${name} $*" | tee -a "$LOG_FILE"
}

# -----------------------------------------------------------------------------
# Puts the message (all arguments) to the log file and to stdout, but without
# a timestamp.

lognotime()
{
    echo "$*" | tee -a "$LOG_FILE"
}

# -----------------------------------------------------------------------------
# Runs printf to the log file and to stdout.

logf()
{
    fmt="$1"
    shift
    echo -n "`elapsed` " | tee -a "$LOG_FILE"
    printf "$1" $* | tee -a "$LOG_FILE"
}

# -----------------------------------------------------------------------------
# Runs printf to the log file and to stdout, but without time.

logfnotime()
{
    fmt="$1"
    shift
    printf "$fmt" $* | tee -a "$LOG_FILE"
}

# -----------------------------------------------------------------------------
# Yeah, this is a little tricky. It's just a way to short-cut setting a
# default value for a variable.

setdefault()
{
    var=`echo $1`
    cmd="echo \$$var"
    val="`eval $cmd`"

    if [ -z "$val" ]; then
        eval $var="$2"
    fi
}

# -----------------------------------------------------------------------------
# Prints then runs the command line passed to it.
# If LOG_FILE is set, logs the result as well.

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

# -----------------------------------------------------------------------------
# Prints then runs the command line passed to it. If the command
# fails with an exit code, this will exit with that code.

run_exit_on_error()
{
    run "$*"
    result=$?

    if [ $result -ne 0 ]; then
        exit $result
    fi

    return 0
}

# -----------------------------------------------------------------------------
# Echoes the elapsed time since the time given in the argument (number of
# seconds since 1970, local time). If no argument is specified, the start
# time will be the time that the script was last included. The output is
# formatted as HH:MM:SS.

elapsed_start=`date "+%s"`

elapsed()
{
    if [ -z "$1" ]; then
        start=$elapsed_start
    else
        start=$1
    fi
    now=`date "+%s"`
    secs=$[now - start]
    mins=$[(secs / 60) % 60]
    hours=$[secs / 3600]
    secs=$[secs % 60]
    printf "%02u:%02u:%02u" $hours $mins $secs
}

# -----------------------------------------------------------------------------
gradd() {
    if [ $# -ne 1 ]; then
        echo ""
        echo "Usage: gradd <git-account-name>"
        echo ""
        echo "Runs 'git remote add [tag] git@[github-url]:<git-account-name>/[repo].git',"
        echo "where [tag], [github-url] and [repo] are determined automatically. The [tag]"
        echo "is made from the initials of <git-account-name>, or from the first two"
        echo "characters."
        echo ""
        echo "For example:"
        echo ""
        echo "   gradd prima-donna     # tag is pd"
        echo "   gradd james-johnson   # tag is jj"
        echo "   gradd elephant        # tag is el"
        echo ""
    else
        account="$1"
        tag=`echo "$account" | sed -s 's+^\(.\).*[-]\(.\).*+\1\2+'`
        tag=${tag:0:2}
        ghub=`git remote -v | grep '^origin.*[(]push[)]' | sed -e 's/^origin.//' -e 's/[:].*$//'`
        repo=`git remote -v | grep '^origin.*[(]push[)]' | sed -e 's+^[^/]*/++' -e 's/ .*$//'`
        cmd="git remote add '$tag' '$ghub:$account/$repo'"
        echo $cmd
        eval $cmd
    fi
}

# -----------------------------------------------------------------------------
# grep-find
# Note that this ignores .svn directories.
function grind()
{
    if [ $# -lt 3 ]; then
	echo "Usage: [GRIND_GREP_OPTS=''] grind path search-pattern file-pattern..."
    else
        path="$1"
        search="\"$2\""
        file="-name \"$3\""

        shift 3
        while [ $# -gt 0 ]; do
            file="$file -o -name \"$1\""
            shift
        done

        if [ -z "$GRIND_GREP_OPTS" ]; then
            GRIND_GREP_OPTS="-n"
        fi

        cmd="find \"$path\" -type f \\( \\( -name .svn -prune \\) -o \\( $file \\) \\) -exec grep $GRIND_GREP_OPTS $search /dev/null {} \\;"
        echo $cmd
        eval $cmd
    fi
}   # grind()

# -----------------------------------------------------------------------------
# grep-find, case insensitive.
# Note that this ignores .svn directories.
function grInd()
{
    if [ $# -lt 3 ]; then
	echo "Usage: [GRIND_GREP_OPTS=''] grInd path search-pattern file-pattern..."
    else
        path="$1"
        search="\"$2\""
        file="-name \"$3\""

        shift 3
        while [ $# -gt 0 ]; do
            file="$file -o -name \"$1\""
            shift
        done

        if [ -z "$GRIND_GREP_OPTS" ]; then
            GRIND_GREP_OPTS="-n"
        fi

        cmd="find \"$path\" -type f \\( \\( -name .svn -prune \\) -o \\( $file \\) \\) -exec grep -i $GRIND_GREP_OPTS $search /dev/null {} \\;"
        echo $cmd
        eval $cmd
    fi
}   # grInd()

# -----------------------------------------------------------------------------
# grep-find on C/C++ source files.
function grindc()
{
    if [ $# -lt 1 -o $# -gt 2 ]; then
        echo "Usage: grindc <regex> [starting-path]"
    else
        path="."
        if [ $# -gt 1 ]; then
            path="$2"
        fi

        grind "$path" "$1" "*.c" "*.c++" "*.cpp" "*.cc"
    fi
}   # grindc()

# -----------------------------------------------------------------------------
# grep-find on C/C++ source files, case insensitive.
function grIndc()
{
    if [ $# -lt 1 -o $# -gt 2 ]; then
        echo "Usage: grIndc <regex> [starting-path]"
    else
        path="."
        if [ $# -gt 1 ]; then
            path="$2"
        fi

        grInd "$path" "$1" "*.c" "*.c++" "*.cpp" "*.cc"
    fi
}   # grIndc()

# -----------------------------------------------------------------------------
# grep-find on C/C++ header files.
function grindh()
{
    if [ $# -lt 1 -o $# -gt 2 ]; then
        echo "Usage: grindh <regex> [starting-path]"
    else
        path="."
        if [ $# -gt 1 ]; then
            path="$2"
        fi

        grind "$path" "$1" "*.h" "*.h++" "*.hpp" "*.hh"
    fi
}   # grindh()

# -----------------------------------------------------------------------------
# grep-find on C/C++ header files, case insensitive.
function grIndh()
{
    if [ $# -lt 1 -o $# -gt 2 ]; then
        echo "Usage: grIndh <regex> [starting-path]"
    else
        path="."
        if [ $# -gt 1 ]; then
            path="$2"
        fi

        grInd "$path" "$1" "*.h" "*.h++" "*.hpp" "*.hh"
    fi
}   # grIndh()

# -----------------------------------------------------------------------------
# grep-find on C/C++ source or header files.
function grindch()
{
    if [ $# -lt 1 -o $# -gt 2 ]; then
        echo "Usage: grindch <regex> [starting-path]"
    else
        path="."
        if [ $# -gt 1 ]; then
            path="$2"
        fi

        grind "$path" "$1" "*.h" "*.h++" "*.hpp" "*.hh" "*.c" "*.c++" "*.cpp" "*.cc"
    fi
}   # grindch()

# -----------------------------------------------------------------------------
# grep-find on C/C++ source or header files, case insensitive.
function grIndch()
{
    if [ $# -lt 1 -o $# -gt 2 ]; then
        echo "Usage: grIndch <regex> [starting-path]"
    else
        path="."
        if [ $# -gt 1 ]; then
            path="$2"
        fi

        grInd "$path" "$1" "*.h" "*.h++" "*.hpp" "*.hh" "*.c" "*.c++" "*.cpp" "*.cc"
    fi
}   # grIndch()

# -----------------------------------------------------------------------------
# grep-find on CMakeLists.txt.
function grindcm()
{
    if [ $# -lt 1 -o $# -gt 2 ]; then
        echo "Usage: grindcm <regex> [starting-path]"
    else
        path="."
        if [ $# -gt 1 ]; then
            path="$2"
        fi

        grind "$path" "$1" "CMakeLists.txt"
    fi
}   # grindcm()

# -----------------------------------------------------------------------------
# grep-find on CMakeLists.txt, case-insensitive.
function grIndcm()
{
    if [ $# -lt 1 -o $# -gt 2 ]; then
        echo "Usage: grIndcm <regex> [starting-path]"
    else
        path="."
        if [ $# -gt 1 ]; then
            path="$2"
        fi

        grInd "$path" "$1" "CMakeLists.txt"
    fi
}   # grIndcm()

# -----------------------------------------------------------------------------
# Removes from the current tree (or $1 if given), recursively, all files
# matching: *~ *.o *.bak
clean()
{
    start_dir="."

    if [ ! -z "$1" ]; then
        start_dir="$1"
        shift
    fi

    run "find \"$start_dir\" \( -name \"*~\" -o -name \"*.[od]\" -o -name \"*.bak\" \) | xargs -t rm -rf"
}   # clean()

# -----------------------------------------------------------------------------
rmbldlib_usage() {
    echo ""
    echo "Usage: rmbldlib [options] <name>...     # In root of build area."
    echo ""
    echo "Deletes the following patterns (all combinations):"
    echo "  libs/(Win32|x64)/(Debug|Release)_Static/(lib|include(|/mx))/(|lib)<name>*"
    echo "  libs/(|mx/|sfSDK)(|lib)<name>*"
    echo "Prints the name of any successfully removed item."
    echo ""
    echo "Options:"
    echo ""
    echo "  -h   Print this help."
    echo "  -c   Run 'cmake -G <generator> -D CMAKE_BUILD_TYPE=<conf> <src>' from cache."
    echo ""
}

# -----------------------------------------------------------------------------
# Removes the build area for the given third party libraries.
rmbldlib() {
    run_cmake=""
    if [ -z "$1" ]; then
        rmbldlib_usage
        return
    fi

    while [ $# -gt 0 ]; do
        name="$1"
        shift
        if [ "$name" == "-h" ]; then
            rmbldlib_usage
            return
        elif [ "$name" == "-c" ]; then
            run_cmake="yes"
        else
            for libname in "$name" "lib$name"; do
                for dir in libs/{Win32,x64}/{Debug,Release}_Static/{lib,include{,/mx}}; do
                    files=`ls "$dir/$libname"* 2> /dev/null`
                    if [ ! -z "$files" ]; then
                        echo "$dir/$libname*"
                        rm -rf "$dir/$libname"*
                    fi
                done
                for dir in sfSDK/libs libs{,/mx}; do
                    files=`ls "$dir/$libname"* 2> /dev/null`
                    if [ ! -z "$files" ]; then
                        echo "$dir/$libname*"
                        rm -rf "$dir/$libname"*
                    fi
                done
            done
        fi
    done

    if [ ! -z "$run_cmake" ]; then
        src=`grep '^CMAKE_HOME_DIRECTORY' CMakeCache.txt | sed -e 's+^.*=C:+/c+' 2> /dev/null`
        gen=`grep '^CMAKE_GENERATOR' CMakeCache.txt | sed -e 's+^.*=++' 2> /dev/null`
        typ=`grep '^CMAKE_BUILD_TYPE' CMakeCache.txt | sed -e 's+^.*=++' 2> /dev/null`
        if [ -z "$src" -o -z "$gen" -o -z "$typ" ]; then
            echo "rmbldlib: could not determine CMAKE_HOME_DIRECTORY or CMAKE_BUILD_TYPE."
        else
            cmd="cmake -G \"$gen\" -D \"CMAKE_BUILD_TYPE=$typ\" \"$src\""
            echo "$cmd"
            eval $cmd
        fi
    fi
}
