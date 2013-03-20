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
# grep-find
# Note that this ignores .svn directories.
function grind()
{
    if [ $# -lt 3 ]; then
	echo "Usage: grind path search-pattern file-pattern..."
    else
        path="$1"
        search="\"$2\""
        file="-name \"$3\""

        shift 3
        while [ $# -gt 0 ]; do
            file="$file -o -name \"$1\""
            shift
        done

        cmd="find \"$path\" \\( \\( -name .svn -prune \\) -o \\( $file \\) \\) -exec grep -n $search /dev/null {} \\;"
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
	echo "Usage: grInd path search-pattern file-pattern..."
    else
        path="$1"
        search="\"$2\""
        file="-name \"$3\""

        shift 3
        while [ $# -gt 0 ]; do
            file="$file -o -name \"$1\""
            shift
        done

        cmd="find \"$path\" \\( \\( -name .svn -prune \\) -o \\( $file \\) \\) -exec grep -i -n $search /dev/null {} \\;"
        echo $cmd
        eval $cmd
    fi
}   # grInd()

# -----------------------------------------------------------------------------
# grep-find on C/C++ source files.
function grindc()
{
    if [ $# -eq 0 ]; then
        echo "Usage: grindc <regex> [starting-path]"
    elif [ $# -gt 2 ]; then
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
    if [ $# -eq 0 ]; then
        echo "Usage: grIndc <regex> [starting-path]"
    elif [ $# -gt 2 ]; then
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
    if [ $# -eq 0 ]; then
        echo "Usage: grindh <regex> [starting-path]"
    elif [ $# -gt 2 ]; then
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
    if [ $# -eq 0 ]; then
        echo "Usage: grIndh <regex> [starting-path]"
    elif [ $# -gt 2 ]; then
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
    if [ $# -eq 0 ]; then
        echo "Usage: grindch <regex> [starting-path]"
    elif [ $# -gt 2 ]; then
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
    if [ $# -eq 0 ]; then
        echo "Usage: grIndch <regex> [starting-path]"
    elif [ $# -gt 2 ]; then
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
