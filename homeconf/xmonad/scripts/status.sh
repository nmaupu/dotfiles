#!/bin/bash

DIRNAME=$(dirname $0)
DATE_INTERVAL=1
CPU_INTERVAL=1

RED="^fg(red)"
GREEN="^fg(green)"
ORANGE="^fg(orange)"
NORMAL="^fg(#ffffff)"

ICON_DIR=${DIRNAME}/../icons

function dateformat
{
  date "+%A %d.%m.%Y %H:%M:%S"
}

function cputemp
{
  TEMP=$1
  TEMP_NUM=$(echo $TEMP | sed -e 's/°C//g')

  if [ ${TEMP_NUM} -ge 60 ]
  then
    COLOR="${RED}"
  elif [ ${TEMP_NUM} -ge 45 ]
  then
    COLOR="${ORANGE}"
  else
    COLOR="${GREEN}"
  fi

  echo "${COLOR}${TEMP}${NORMAL}"
}

function print_status
{
  if [ ! $DATE_CPT ]
  then
    export DATE_CPT=${DATE_INTERVAL}
  else
    export DATE_CPT=$DATE_CPT
  fi
  if [ ! ${CPU_CPT} ]
  then
    export CPU_CPT=${CPU_INTERVAL}
  else
    export CPU_CPT=${CPU_CPT}
  fi
  
  if [ $DATE_CPT -ge $DATE_INTERVAL ]
  then
    PDATE=$(dateformat)
    DATE_CPT=0
  fi

  ##if [ $CPU_CPT -ge $CPU_INTERVAL ]
  ##then
  ##  TEMP=$(sensors | grep temp | tr -s " " | awk '{print $2}' | tr -d "+")
  ##  TEMP_1=$(echo -e "$TEMP" | head -1)
  ##  TEMP_2=$(echo -e "$TEMP" | tail -1)
  ##  PCPU="$(cputemp ${TEMP_1})/$(cputemp ${TEMP_2})"
  ##  CPU_CPT=0
  ##fi

  #echo "^fg($GREEN)${PCPU} ^fg($NORMAL) ^p(3)^r(3x3)^p(3) $PDATE"
  echo "${PCPU}$NORMAL ^i(${ICON_DIR}/separator.xbm) $PDATE"
  DATE_CPT=$((${DATE_CPT}+1))
  CPU_CPT=$((${CPU_CPT}+1))
}
