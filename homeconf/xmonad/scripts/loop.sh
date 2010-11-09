#!/bin/bash

INTERVAL=1
DIRNAME=$(dirname $0)

while [ true ]
do
  source $DIRNAME/status.sh
  print_status
  sleep $INTERVAL
done
