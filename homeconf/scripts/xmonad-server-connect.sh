#!/bin/bash

DIRNAME=`dirname $0`
. ${DIRNAME}/easyopt/easyopt.sh

IPS_DIR=/dir/to/ips/files

usage() {
  cat << EOF
Usage: $0 <options>
 -l   list all available conf
 -e   execute one particularly configuration
 -h   displays this help and exits
EOF
}

list() {
  ls -1 ${IPS_DIR} | sed -e 's/\.ip//g'
}

execute() {
  PARAM=$1
  cssh `cat ${IPS_DIR}/${1}.ip`
}

easyopt_add "l" 'list && exit 0'
easyopt_add "e:" 'execute "$OPTARG" && exit 0'
easyopt_add "h" 'usage && exit 0'
easyopt_parse_opts "$@"

usage && exit 0
