#!/bin/bash

script_name=$(basename "$0")

if [ $(id -u) -eq 0 ]; then
    echo "Don't run me as root. I have 'sudo' built in."
    exit 1
fi

if [ -z "$1" ]; then
    echo ""
    echo "$script_name mounts a VirtualBox share in the user's home directory."
    echo ""
    echo "Usage: $script_name <sharename>" 1>&2
    exit 1
    echo ""
fi

run() {
    echo "$@"
    eval "$@"
}

run mkdir -p "$HOME/$1"
run sudo mount -t vboxsf -o uid=$(id -u),gid=$(id -g) "$1" "$HOME/$1"
