# Useful (ba)sh functions. 

# -----------------------------------------------------------------------------
# Puts the message (all arguments) to $LOG_FILE and to stdout.
# If set, uses PROC_NAME, LOG_ELAPSED_TIME, and LOG_TIME_FORMAT.
# See the code for details. It ain't that hard.

log()
{
    if [[ -z "$LOG_FILE" ]] ; then
        LOG_FILE=/tmp/build.log
    fi

    if [[ -z "$PROC_NAME" ]] ; then
        name=""
    else
        name=" $PROC_NAME"
    fi

    if [[ "$LOG_ELAPSED_TIME" = "yes" ]] ; then
        timestamp=`elapsed`
    elif [[ ! -z "$LOG_TIME_FORMAT" ]] ; then
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

    if [[ -z "$val" ]]; then
        eval $var="$2"
    fi
}

# -----------------------------------------------------------------------------
# Prints then runs the command line passed to it.
# If LOG_FILE is set, logs the result as well.

run()
{
    cmd="$*"

    if [[ -z "$LOG_FILE" ]] ; then
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

    if [[ $result -ne 0 ]]; then
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
    if [[ -z "$1" ]]; then
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
# Removes from the current tree (or $1 if given), recursively, all files
# matching: *~ *.o *.bak
clean()
{
    start_dir="."

    if [[ ! -z "$1" ]]; then
        start_dir="$1"
        shift
    fi

    run "find \"$start_dir\" \( -iname \"*~\" -o -iname \"*.[od]\" -o -iname \"*.bak\" \) | xargs -t rm -rf"
}   # clean()

# -----------------------------------------------------------------------------
gradd() {
    if [[ $# -ne 1 ]]; then
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
grorig() {
    if [[ -z "$#" ]]; then
        grorig .
    fi
    if [[ "$1" == "-h" ]]; then
        echo ""
        echo "Usage: grorig [dir...]"
        echo ""
        echo "Runs 'git remote -v | grep ^origin | head -1' on each argument, or on '.' if"
        echo "no argument is given."
        echo ""
        echo "   -h      Show this help"
        echo "   -v      Show verbose information"
        echo ""
    else
        verbose=0
        if [[ -z "$1" ]]; then
            grorig '.'
        else
            while [[ $# -gt 0 ]]; do
                dir="$1"
                shift
                if [[ "$dir" == "-v" ]]; then
                    verbose=1
                    continue
                fi
                if [[ -d "$dir" ]]; then
                    printf "%-20s" "$dir: "
                    cmd="(cd '$dir'; git remote -v | grep ^origin | head -1 2> /dev/null)"
                    if [[ $verbose -ne 0 ]]; then
                        echo "grorig: cmd=\"$cmd\""
                    fi
                    eval "$cmd"
                    if [[ $? -ne 0 ]]; then
                        echo "not git repo"
                    fi
                elif [[ $verbose -ne 0 ]]; then
                    echo "grorig: '$dir' is not a directory."
                fi
            done
        fi
    fi
}

# -----------------------------------------------------------------------------
# find-grep usage.
function fig_usage() {
    cat <<EOF

    Usage: fig [-Hviechmpsl] [-f FILE_GLOB] [options...] REGEX [PATH...]

        'fig' performs a find-grep combination to search files for a regular
        expression.

        The given REGEX is sought in file matching the various '-f FILE_GLOB'
        options (or short-cuts like '-c') in each PATH. If no PATH is provided,
        '*' is used. If FILE_GLOB is provided, '*' is used.

        The default value for PRUNE_GLOB is '-P .git -P .svn', meaning that
        those subdirectories will not be traversed when finding source files to
        search.

    OPTIONS

        -H              Show this usage information.
        -v              Print the command that will be run before running it.

        -F FILE_GLOB    Globs to match for files [default is '*'].
        -c              C-like files: '*.c' '*.cc' '*.cpp' '*.c++' '*.cs'
        -h              Header files: '*.h' '*.hh' '*.hpp' '*.h++'
        -m              Make files: 'Makefile' 'CMakeLists.txt' '*.mk'
        -p              Python source: '*.py'
        -s              Script source: '*.sh' '*.py' '*.lua' '*.sch'
        -j              Java-ish files: '*.java' '*.js' '*.clj'

        -G GREP_OPTS    Specify other options to pass to 'grep'.
        -i              Perform case-insensitive search; same as '-g -i'.
        -e              Use egrep-style regular expression; same as '-g -e'.

        -P PRUNE_GLOB   Glob to match subdirectory names *not* to search.

EOF
}

# -----------------------------------------------------------------------------
# find-grep.
function fig() {
    VERBOSE=0
    FILE_GLOBS=()
    GREP_OPTS=()
    PRUNE_GLOBS=(.git .svn)
    PATHS=()

    OPTIND=1

    while getopts "HvF:chmpsjG:ieP:" opt; do
        case $opt in
            H)
                fig_usage
                return 0
                ;;
            v)
                VERBOSE=1
                ;;
            F)
                FILE_GLOBS=(${FILE_GLOBS[@]} "'$OPTARG'")
                ;;
            c)
                FILE_GLOBS=(${FILE_GLOBS[@]} "'*.c'" "'*.cc'" "'*.cpp'" "'*.c++'" "'*.cs'")
                ;;
            h)
                FILE_GLOBS=(${FILE_GLOBS[@]} "'*.h'" "'*.hh'" "'*.hpp'" "'*.h++'")
                ;;
            m)
                FILE_GLOBS=(${FILE_GLOBS[@]} "'Makefile'" "'CMakeLists.txt'" "'*.mk'")
                ;;
            p)
                FILE_GLOBS=(${FILE_GLOBS[@]} "'*.py'")
                ;;
            s)
                FILE_GLOBS=(${FILE_GLOBS[@]} "'*.sh'" "'*.py'" "'*.lua'" "'*.sch'")
                ;;
            j)
                FILE_GLOBS=(${FILE_GLOBS[@]} "'*.java'" "'*.js'" "'*.clj'")
                ;;
            G)
                GREP_OPTS=(${GREP_OPTS[@]} "'$OPTARG'")
                ;;
            i)
                GREP_OPTS=(${GREP_OPTS[@]} -i)
                ;;
            e)
                GREP_OPTS=(${GREP_OPTS[@]} -e)
                ;;
            P)
                PRUNE_GLOBS=(${PRUNE_GLOBS[@]} "'$OPTARG'")
                ;;
        esac
    done

    shift $[OPTIND - 1]

    if [ $# -lt 1 ]; then
        echo "fig: no REGEX provided; use '-H' for usage" 1>&2
        return 1
    fi

    REGEX="$1"
    shift

    while [ $# -gt 0 ]; do
        PATHS=(${PATHS[@]} "$1")
        shift
    done

    if [ ${#PATHS[@]} -eq 0 ]; then
        PATHS=(.)
    fi

    if [ ${#FILE_GLOBS[@]} -eq 0 ]; then
        FILE_GLOBS=("'*'")
    fi

    prune_args=""

    # Add directories to be pruned.
    for ((i=0; i<${#PRUNE_GLOBS[@]}; i++)); do
        if [ -n "$prune_args" ]; then
            prune_args="$prune_args -o "
        fi
        prune_args="${prune_args}-name ${PRUNE_GLOBS[$i]}"
    done

    if [ -n "$prune_args" ]; then
        prune_args="\\( $prune_args \\) -prune -o "
    fi

    file_args=""

    # Add files to be searched.
    for ((i=0; i<${#FILE_GLOBS[@]}; i++)); do
        if [ -n "$file_args" ]; then
            file_args="$file_args -o "
        fi
        file_args="${file_args}-iname ${FILE_GLOBS[$i]}"
    done

    cmd="find ${PATHS[@]} ${prune_args}\\( -type f -a \\( $file_args \\) \\) -exec grep -n ${GREP_OPTS[@]} '$REGEX' /dev/null {} \\;"

    if [ $VERBOSE -ne 0 ]; then
        echo "$cmd"
    fi

    eval "$cmd"
}   # fig()
    
# -----------------------------------------------------------------------------
# grep-find
# Note that this ignores .svn directories.
function grind()
{
    if [[ $# -lt 3 ]]; then
	echo "Usage: [GRIND_GREP_OPTS=''] grind path search-pattern file-pattern..."
    else
        path="$1"
        search="\"$2\""
        file="-iname \"$3\""

        shift 3
        while [[ $# -gt 0 ]]; do
            file="$file -o -iname \"$1\""
            shift
        done

        if [[ -z "$GRIND_GREP_OPTS" ]]; then
            GRIND_GREP_OPTS="-n"
        fi

        cmd="find \"$path\" -type f \\( \\( -iname .svn -prune \\) -o \\( $file \\) \\) -exec grep $GRIND_GREP_OPTS $search /dev/null {} \\;"
        echo $cmd
        eval $cmd
    fi
}   # grind()

# -----------------------------------------------------------------------------
# grep-find, case insensitive.
# Note that this ignores .svn directories.
function grInd()
{
    if [[ $# -lt 3 ]]; then
	echo "Usage: [GRIND_GREP_OPTS=''] grInd path search-pattern file-pattern..."
    else
        path="$1"
        search="\"$2\""
        file="-iname \"$3\""

        shift 3
        while [[ $# -gt 0 ]]; do
            file="$file -o -iname \"$1\""
            shift
        done

        if [[ -z "$GRIND_GREP_OPTS" ]]; then
            GRIND_GREP_OPTS="-n"
        fi

        cmd="find \"$path\" -type f \\( \\( -iname .svn -prune \\) -o \\( $file \\) \\) -exec grep -i $GRIND_GREP_OPTS $search /dev/null {} \\;"
        echo $cmd
        eval $cmd
    fi
}   # grInd()

# -----------------------------------------------------------------------------
# grep-find on C/C++ source files.
function grindc()
{
    if [[ $# -lt 1 || $# -gt 2 ]]; then
        echo "Usage: grindc <regex> [starting-path]"
    else
        path="."
        if [[ $# -gt 1 ]]; then
            path="$2"
        fi

        grind "$path" "$1" "*.c" "*.c++" "*.cpp" "*.cc" "*.cs"
    fi
}   # grindc()

# -----------------------------------------------------------------------------
# grep-find on C/C++ source files, case insensitive.
function grIndc()
{
    if [[ $# -lt 1 || $# -gt 2 ]]; then
        echo "Usage: grIndc <regex> [starting-path]"
    else
        path="."
        if [[ $# -gt 1 ]]; then
            path="$2"
        fi

        grInd "$path" "$1" "*.c" "*.c++" "*.cpp" "*.cc" "*.cs"
    fi
}   # grIndc()

# -----------------------------------------------------------------------------
# grep-find on C/C++ header files.
function grindh()
{
    if [[ $# -lt 1 || $# -gt 2 ]]; then
        echo "Usage: grindh <regex> [starting-path]"
    else
        path="."
        if [[ $# -gt 1 ]]; then
            path="$2"
        fi

        grind "$path" "$1" "*.h" "*.h++" "*.hpp" "*.hh"
    fi
}   # grindh()

# -----------------------------------------------------------------------------
# grep-find on C/C++ header files, case insensitive.
function grIndh()
{
    if [[ $# -lt 1 || $# -gt 2 ]]; then
        echo "Usage: grIndh <regex> [starting-path]"
    else
        path="."
        if [[ $# -gt 1 ]]; then
            path="$2"
        fi

        grInd "$path" "$1" "*.h" "*.h++" "*.hpp" "*.hh"
    fi
}   # grIndh()

# -----------------------------------------------------------------------------
# grep-find on C/C++ source or header files.
function grindch()
{
    if [[ $# -lt 1 || $# -gt 2 ]]; then
        echo "Usage: grindch <regex> [starting-path]"
    else
        path="."
        if [[ $# -gt 1 ]]; then
            path="$2"
        fi

        grind "$path" "$1" "*.h" "*.h++" "*.hpp" "*.hh" "*.c" "*.c++" "*.cpp" "*.cc"
    fi
}   # grindch()

# -----------------------------------------------------------------------------
# grep-find on C/C++ source or header files, case insensitive.
function grIndch()
{
    if [[ $# -lt 1 || $# -gt 2 ]]; then
        echo "Usage: grIndch <regex> [starting-path]"
    else
        path="."
        if [[ $# -gt 1 ]]; then
            path="$2"
        fi

        grInd "$path" "$1" "*.h" "*.h++" "*.hpp" "*.hh" "*.c" "*.c++" "*.cpp" "*.cc"
    fi
}   # grIndch()

# -----------------------------------------------------------------------------
# grep-find on CMakeLists.txt.
function grindcm()
{
    if [[ $# -lt 1 || $# -gt 2 ]]; then
        echo "Usage: grindcm <regex> [starting-path]"
    else
        path="."
        if [[ $# -gt 1 ]]; then
            path="$2"
        fi

        grind "$path" "$1" "CMakeLists.txt"
    fi
}   # grindcm()

# -----------------------------------------------------------------------------
# grep-find on CMakeLists.txt, case-insensitive.
function grIndcm()
{
    if [[ $# -lt 1 || $# -gt 2 ]]; then
        echo "Usage: grIndcm <regex> [starting-path]"
    else
        path="."
        if [[ $# -gt 1 ]]; then
            path="$2"
        fi

        grInd "$path" "$1" "CMakeLists.txt"
    fi
}   # grIndcm()

# -----------------------------------------------------------------------------
# ls -ltr <args>... | tail <any-#-args>
lsl() {
    ls_args=( )
    tail_arg="-20"
    for arg in $@; do
        case $arg in
            -[0-9]*)
                tail_arg=$arg
                ;;
            *)
                ls_args=($ls_args "$arg")
                ;;
        esac
    done
    ls -ltr $ls_args | tail $tail_arg
}

# -----------------------------------------------------------------------------
# mkdir-chdir
function mcd()
{
    mkdir -p "$1"
    cd "$1"
}

# -----------------------------------------------------------------------------
# Runs a command with SIGINT untrapped by the shell.

noint()
{
    trap '' 2
    eval "$*"
    trap 2
}

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
    if [[ -z "$1" ]]; then
        rmbldlib_usage
        return
    fi

    while [[ $# -gt 0 ]]; do
        name="$1"
        shift
        if [[ "$name" == "-h" ]]; then
            rmbldlib_usage
            return
        elif [[ "$name" == "-c" ]]; then
            run_cmake="yes"
        else
            for libname in "$name" "lib$name"; do
                for dir in libs/{Win32,x64}/{Debug,Release}_Static/{lib,include{,/mx}}; do
                    files=`ls "$dir/$libname"* 2> /dev/null`
                    if [[ ! -z "$files" ]]; then
                        echo "$dir/$libname*"
                        rm -rf "$dir/$libname"*
                    fi
                done
                for dir in sfSDK/libs libs{,/mx}; do
                    files=`ls "$dir/$libname"* 2> /dev/null`
                    if [[ ! -z "$files" ]]; then
                        echo "$dir/$libname*"
                        rm -rf "$dir/$libname"*
                    fi
                done
            done
        fi
    done

    if [[ ! -z "$run_cmake" ]]; then
        src=`grep '^CMAKE_HOME_DIRECTORY' CMakeCache.txt | sed -e 's+^.*=C:+/c+' 2> /dev/null`
        gen=`grep '^CMAKE_GENERATOR' CMakeCache.txt | sed -e 's+^.*=++' 2> /dev/null`
        typ=`grep '^CMAKE_BUILD_TYPE' CMakeCache.txt | sed -e 's+^.*=++' 2> /dev/null`
        if [[ -z "$src" || -z "$gen" || -z "$typ" ]]; then
            echo "rmbldlib: could not determine CMAKE_HOME_DIRECTORY or CMAKE_BUILD_TYPE."
        else
            cmd="cmake -G \"$gen\" -D \"CMAKE_BUILD_TYPE=$typ\" \"$src\""
            echo "$cmd"
            eval $cmd
        fi
    fi
}   # rmbldlib()

# -----------------------------------------------------------------------------
# Calculates the parallel resistance of a N resistors.
respar() {
    if [[ -z "$1" ]]; then
        echo "calculate the combined parallel resistance of N resistors."
        echo "usage: respar <R1> ... [RN]"
        exit 1
    else
        r=$[$1 * 100]
        shift
        while [[ $# -gt 0 ]]; do
            s=$[$1 * 100]
            shift
            r=$[ r * s / (r + s) ]
        done
        echo $[(r + 50) / 100]
    fi
}   # respar()
