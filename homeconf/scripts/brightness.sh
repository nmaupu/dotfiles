#!/bin/bash

[ $# -ne 1 ] && exit 1

MAX=5000

PERCENT=${1}
VALUE=$((${MAX}*${PERCENT}/100))
echo ${VALUE} > /sys/class/backlight/intel_backlight/brightness

exit 0
