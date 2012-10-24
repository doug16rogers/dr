#! /bin/bash
while [ -t ] ; do
  echo -e -n "\r`hexdate`"
done
