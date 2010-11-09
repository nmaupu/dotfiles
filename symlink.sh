#!/usr/bin/env bash

DIRNAME=$(dirname $0)
DIRNAME_ABS=$(readlink -f ${DIRNAME})
CONFDIR=${DIRNAME_ABS}/homeconf

relink() {
  rm -ri $1
  ln -sn $2 $1
}

process_files() {
  for file in $(ls -1 ${CONFDIR})
  do
    relink ~/.${file} ${CONFDIR}/${file}
  done
}

process_files
