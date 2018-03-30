#!/bin/bash

script=$(basename "$0")

export DR_OS="$HOME/dr/os"

if [[ ! -d "$DR_OS" ]]; then
    echo "$sciprt: source OS directory '$DR_OS' does not exist."
    exit 1
fi

for file in "$DR_OS"/home-*; do
    dotfile="$(echo $file | sed -e 's/^.*home-//' -e 's/~//')"
    echo \
    cp \"$file\" \"$HOME/$dotfile\"
    cp "$file" "$HOME/$dotfile"
done
