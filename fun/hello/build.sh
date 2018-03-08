#!/bin/bash
build () {
    lang="$1"
    checker="$2"
    build="$3"
    run="$4"
    if [ -z "$run" ]; then
        run="./$lang-hello"
    fi
    echo ""
    echo "# $lang..."
    eval "$checker" > /dev/null 2>&1
    if [ $? -eq 0 ] ; then
        echo "$build > $lang-build.log 2>&1"
        eval "$build > $lang-build.log 2>&1"
        if [ $? -eq 0 ]; then
            echo "$run"
            eval "$run"
        else
            echo "**** build failed for $lang ****"
        fi
    else
        echo "**** skipping $lang because '$checker' failed ****"
    fi
}

# This is needed for Ada. *sigh*
use_ada_compiler() {
    host=`uname -n`
    host=${host:0:11}
    if [ "$host" == "drogers-mbp" ]; then
        # Add the gnuada compiler downloaded from:
        #   https://sourceforge.net/projects/gnuada/files/GNAT_GCC%20Mac%20OS%20X/6.1.0/
        export origpath="$PATH"
        export PATH="/Users/drogers/opt/gcc-6.1.0-x86_64-apple-darwin15-2016-bin/bin:$PATH"
        export origdyld="$DYLD_LIBRARY_PATH"
        export DYLD_LIBRARY_PATH="/Users/drogers/opt/gcc-6.1.0-x86_64-apple-darwin15-2016-bin/lib:$DYLD_LIBRARY_PATH"
    fi
}

# Undo the above.
unuse_ada_compiler() {
    host=`uname -n`
    host=${host:0:11}
    if [ "$host" == "drogers-mbp" ]; then
        export PATH="$origpath"
        export DYLD_LIBRARY_PATH="$origdyld"
    fi
}

use_ada_compiler
build 'ada'     'gnatmake --version'  'gnatmake -o ada-hello ada-hello'
unuse_ada_compiler
build 'bf'      './bf.sh --version'   './bf.sh bf-hello bf-hello.bf'
build 'c'       'gcc --version'       'gcc -o c-hello c-hello.c'
build 'c++'     'g++ --version'       'g++ -o c++-hello c++-hello.c++'
use_ada_compiler
build 'fortran' 'gfortran --version'  'gfortran -o fortran-hello fortran-hello.for'
unuse_ada_compiler
build 'go'      'go version'          'go build go-hello.go'
build 'haskell' 'ghc --version'       'ghc -o haskell-hello haskell-hello.hs'
build 'lua'     'lua -v'              'true' 'lua lua-hello.lua'
build 'nim'     'nim -v'              'nim c nim_hello.nim' './nim_hello'
build 'ocaml'   'ocamlbuild -version' 'ocamlbuild -no-links -no-hygiene ocaml-hello.native' '_build/ocaml-hello.native'
build 'scheme'  'guile --version'     'true' 'guile scheme-hello.scm'
build 'sh'      'bash --version'      'true' 'bash sh-hello.sh'
