#! /bin/bash
while [ -t ] ; do
  echo -e -n "`hexdate --format=days+`\33[2A\r"
done
