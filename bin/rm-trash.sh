#!/bin/bash

echo 'rm -rf ~/.local/share/Trash/*'
rm -rf ~/.local/share/Trash/*

dirs=/
uid=$(id -u)

for d in /media/user/* /media/veracrypt*; do
    dt="$d/.Trash-$uid"
    if [ -e "$d" ]; then
        echo "rm -rf $dt"
        rm -rf "$dt"
        dirs="$dirs $d"
    fi
done

echo "df -h $dirs"
df -h $dirs
