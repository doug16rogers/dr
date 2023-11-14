#!/bin/bash

# Copyright (c) 2016-2019 Doug Rogers under the Zero Clause BSD License.
# You are free to do whatever you want with this software. See LICENSE.txt.

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

apt_install_ada() {
    sudo apt install -y gnat
}

apt_install_c() {
    sudo apt install -y build-essential gcc
}

apt_install_cplusplus() {
    sudo apt install -y g++
}

apt_install_fortran() {
    sudo apt install -y gfortran
}

apt_install_go() {
    sudo apt install -y golang
}

apt_install_haskell() {
    sudo apt install -y ghc
}

apt_install_lua() {
    sudo apt install -y lua5.4
}

snap_install_nim() {
    sudo snap install nim-lang --classic
}

apt_install_nodejs() {
    sudo apt install -y nodejs
}

apt_install_ocaml() {
    sudo apt install -y ocaml ocamlbuild
}

apt_install_rust() {
    apt_install_c
    sudo apt install -y curl make
    # Yeah, this is interactive and installs to local ~/.cargo, which must be
    # put into your path. Ugh.
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
}

apt_install_scheme() {
    sudo apt install -y guile3.0
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
build 'nim'     'nim-lang.nim -v'     'nim-lang.nim c nim_hello.nim' './nim_hello'
build 'nodejs'  'node --version'      'nodejs node-hello.js &' 'sleep 1; wget -q -O - http://127.0.0.1:3030/; kill %1'
build 'ocaml'   'ocamlbuild -version' 'ocamlbuild -no-links -no-hygiene ocaml-hello.native' '_build/ocaml-hello.native'
build 'rust'    'rustc --version'     'rustc rust-hello.rs'
build 'scheme'  'guile --version'     'true' 'guile scheme-hello.scm'
build 'sh'      'bash --version'      'true' 'bash sh-hello.sh'
