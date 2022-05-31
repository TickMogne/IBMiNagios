#!/bin/sh

#
# ibminagios.sh
#
# Created by: github.com/TickMogne, 2022.05.31
#

PROGPATH=$(echo "$0" | sed -e 's,[\\/][^\\/][^\\/]*$,,')
. "$PROGPATH"/utils.sh

CONFIGFILE=/etc/ibminagios.conf

HOST=$1

if [ "$HOST" = "" ]; then
  echo "Usage:"
  echo "  ibminagios.sh <host> <command>"
  echo "    command: [ syssts aspused <warning_condition> <critical_condition> ] |"
  echo "             [ job [ msgw <warning_condition> <critical_condition> <exception_of_job_names> ] |"
  echo "             [ act <warning_condition> <critical_condition> <job_name> ] ] |"
  echo "             [ outq splfcount <warning_condition> <critical_condition> <outq_name> ] |"
  echo "             [ sql - <warning_condition> <critical_condition> <sql_statement> ] |"
  echo "             [ sbs [ act <warning_condition> <critical_condition> <sbs_name> ] |"
  echo "                   [ jobcount <warning_condition> <critical_condition> <sbs_name> ] ] |"
  echo "             [ msgq [ inq <warning_condition> <critical_condition> <msgq_name> <msgid | *ALL> <minutes> ] |"
  echo "                    [ search <warning_condition> <critical_condition> <msgq_name> <msgid | *ALL> <minutes> ] ]"
  echo "    warning_condition and critical_condition:"
  echo "      format: [ <condition><value> | - ]"
  echo "        -  : no condition processing"
  echo "      conditions:"
  echo "        lt : less than" 
  echo "        le : less equal" 
  echo "        eq : equal" 
  echo "        ne : not equal" 
  echo "        ge : greater equal"
  echo "        gt : greater than"
  echo "      special value: <NULL>"
  echo "    exception_of_job_names: Job names separated with comma"
  echo "    exception_of_msgid: Job names separated with comma"
  exit 0
fi

CMD=$2
TEMPFILE=`mktemp`
TEMPFILELOG=`mktemp`
STATE=$STATE_OK

while read LINE
do
 IP=`echo $LINE | sed -e 's/^\([0-9]*\.[0-9]*\.[0-9]*\.[0-9]*\):AUTHKEY=.*/\1/'`
 if [ "$IP" = "$HOST" ]; then
   AUTHKEY=`echo $LINE | sed -e 's/^[0-9]*\.[0-9]*\.[0-9]*\.[0-9]*:AUTHKEY=\(.*\)$/\1/'`
 fi
 IP=`echo $LINE | sed -e 's/^\([0-9]*\.[0-9]*\.[0-9]*\.[0-9]*\):HTTPPORT=.*/\1/'`
 if [ "$IP" = "$HOST" ]; then
   HTTPPORT=`echo $LINE | sed -e 's/^[0-9]*\.[0-9]*\.[0-9]*\.[0-9]*:HTTPPORT=\(.*\)$/\1/'`
 fi
 if [ "$HTTPPORT" = "" ]; then
   HTTPPORT=80
 fi
done < $CONFIGFILE

URL="http://$HOST:$HTTPPORT/ibminagios?authkey=$AUTHKEY"

check_value() {
  # - lt le eq ne ge gt
  local a
  local t

  if [ "$1" = "-" ]; then
    return 0
  fi

  VALUE=`echo $1 | sed -e 's/^[a-z][a-z]\(.*\)/\1/'`
  T=`echo $VALUE | grep '^[0-9]$'`
  if [ "$T" = "" ]; then
    T="STRING"
  else
    T="NUMBER"
  fi

  a=`echo $1 | grep '^lt'` 
  if [ "$a" != "" ]; then
    VALUE=`echo $1 | sed -e 's/^lt\(.*\)$/\1/'`
    case $T in
      NUMBER)
        if [ $2 -lt $VALUE ]; then
          return 1
        fi
        ;;
      STRING)
        if [ "$2" < "$VALUE" ]; then
          return 1
        fi
        ;;
    esac
    return 0
  fi

  a=`echo $1 | grep '^le'` 
  if [ "$a" != "" ]; then
    VALUE=`echo $1 | sed -e 's/^le\(.*\)$/\1/'`
    case $T in
      NUMBER)
        if [ $2 -le $VALUE ]; then
          return 1
        fi
        ;;
      STRING)
        if [[ "$VALUE" > "$2" ]]; then
          return 1
        fi
        ;;
    esac
    return 0
  fi

  a=`echo $1 | grep '^eq'` 
  if [ "$a" != "" ]; then
    VALUE=`echo $1 | sed -e 's/^eq\(.*\)$/\1/'`
    case $T in
      NUMBER)
        if [ $2 -eq $VALUE ]; then
          return 1
        fi
        ;;
      STRING)
        if [ "$2" = "$VALUE" ]; then
          return 1
        fi
        ;;
    esac
    return 0
  fi

  a=`echo $1 | grep '^ne'` 
  if [ "$a" != "" ]; then
    VALUE=`echo $1 | sed -e 's/^ne\(.*\)$/\1/'`
    case $T in
      NUMBER)
        if [ $2 -ne $VALUE ]; then
          return 1
        fi
        ;;
      STRING)
        if [ "$2" != "$VALUE" ]; then
          return 1
        fi
        ;;
    esac
    return 0
  fi

  a=`echo $1 | grep '^ge'` 
  if [ "$a" != "" ]; then
    VALUE=`echo $1 | sed -e 's/^ge\(.*\)$/\1/'`
    case $T in
      NUMBER)
        if [ $2 -ge $VALUE ]; then
          return 1
        fi
        ;;
      STRING)
        if [[ "$VALUE" < "$2" ]]; then
          return 1
        fi
        ;;
    esac
    return 0
  fi

  a=`echo $1 | grep '^gt'` 
  if [ "$a" != "" ]; then
    VALUE=`echo $1 | sed -e 's/^gt\(.*\)$/\1/'`
    case $T in
      NUMBER)
        if [ $2 -gt $VALUE ]; then
          return 1
        fi
        ;;
      STRING)
        if [ "$2" > "$VALUE" ]; then
          return 1
        fi
        ;;
    esac
    return 0
  fi

  return 0
}

check() {
  # check CIRITAL first
  check_value $2 $3
  RET=$?

  # if CRITICAL
  if [ $RET -eq 1 ]; then
    return $STATE_CRITICAL
  else
    # check WARNING
    check_value $1 $3
    RET=$?

    # if WARNING
    if [ $RET -eq 1 ]; then
      return $STATE_WARNING
    fi
  fi;

  return $STATE_OK
}

check_result() {
  ERROR=`grep ' ERROR [0-9]*: ' $TEMPFILELOG`
  if [ "$ERROR" != "" ]; then
    echo $ERROR
    return 1
  fi

  RESULT=`grep 'Result' $TEMPFILE | sed -e 's/^Result=\(.*\)\r$/\1/'`
  if [ "$RESULT" != "OK" ]; then
    INFO=`grep 'Info' $TEMPFILE | sed -e 's/^Info=\(.*\)\r$/\1/'`
    echo $INFO
    return 1
  fi

  return 0
}

case $CMD in

  syssts)
    URL="$URL&cmd=001"
    wget -O $TEMPFILE -o $TEMPFILELOG "$URL"
    check_result
    if [ $? -eq 1 ]; then
      STATE=$STATE_UNKNOWN
    else
      SUBCMD=$3

      case $SUBCMD in

        aspused)
          INFO=`grep 'Info' $TEMPFILE | sed -e 's/^Info=*.|AspUsed=\(.*\)|.*$/\1/'`
          if [ "$INFO" = "" ]; then
            STATE=$STATE_UNKNOWN
          else
            check $4 $5 $INFO
            STATE=$?
            echo "$INFO%"
          fi
          ;;

        *)
          STATE=$STATE_UNKNOWN
          echo "Unknown subcommand $SUBCMD"
          ;;

      esac
    fi
    ;;

  job)
    SUBCMD=$3

    URL="$URL&cmd=003&status=*ACTIVE"
    if [ "$SUBCMD" = "act" ]; then
       URL="$URL&jobname=$6"
    fi

    wget -O $TEMPFILE -o $TEMPFILELOG "$URL"
    check_result
    if [ $? -eq 1 ]; then
      exit $STATE_UNKNOWN
    else

      case $SUBCMD in

        msgw)
          if [ "$6" != "" ]; then
            IFS=',' read -ra EXCEPTIONS <<< "$6"
          fi
          C=0
          while read LINE
          do
            INFO=`echo $LINE | grep '^Info=|'`
            if [ "$INFO" != "" ]; then
              ACTIVEJOBSTATUS=`echo $INFO | sed -e 's/^Info=.*|ActiveJobStatus=\(.*\)|SubsystemName=.*$/\1/'`
              if [ "$ACTIVEJOBSTATUS" = "MSGW" ]; then
                JOBNAME=`echo $INFO | sed -e 's/^Info=.*|JobName=\(.*\)|JobUser=.*$/\1/'`
                if [ "$EXCEPTIONS" = "" ]; then
                  let C=C+1
                else
                  INARRAY=$(echo " ${EXCEPTIONS[@]} " | grep -o " $JOBNAME " | wc -w)
                  if [ $INARRAY -eq 0 ]; then
                    let C=C+1
                  fi
                fi
              fi
            fi
          done < $TEMPFILE
          check $4 $5 $C
          STATE=$?
          echo -n $C
          if [ "$6" != "" ]; then
            echo ", exception: $6"
          else
            echo ""
          fi
          ;;

        act)
          INFO=`grep '^Info=|' $TEMPFILE | wc -l`
          check $4 $5 $INFO
          STATE=$?
          echo "$INFO"
          ;;

        *)
          STATE=$STATE_UNKNOWN
          echo "Unknown subcommand $SUBCMD"
          ;;

      esac
    fi
    ;;

  outq)
    URL="$URL&cmd=002&outqname=$6"
    wget -O $TEMPFILE -o $TEMPFILELOG "$URL"
    check_result
    if [ $? -eq 1 ]; then
      STATE=$STATE_UNKNOWN
    else
      SUBCMD=$3
      case $SUBCMD in

        splfcount)
          INFO=`grep 'Info' $TEMPFILE | sed -e 's/^Info=.*|NumberOfFiles=\(.*\)|.*$/\1/'`
          if [ "$INFO" = "" ]; then
            STATE=$STATE_UNKNOWN
          else
            check $4 $5 $INFO
            STATE=$?
            echo "$INFO"
          fi
          ;;

        *)
          STATE=$STATE_UNKNOWN
          echo "Unknown subcommand $SUBCMD"
          ;;

      esac
    fi
    ;;

  sql)
    URL="$URL&cmd=004&sql=$6"
    wget -O $TEMPFILE -o $TEMPFILELOG "$URL"
    check_result
    if [ $? -eq 1 ]; then
      STATE=$STATE_UNKNOWN
    else
      INFO=`grep 'Info' $TEMPFILE | sed -e 's/^Info=.*|Value=\(.*\)|.*$/\1/'`
      check $4 $5 $INFO
      STATE=$?
      echo "$INFO"
    fi
    ;;

  sbs)
    URL="$URL&cmd=005&sbsname=$6"
    wget -O $TEMPFILE -o $TEMPFILELOG "$URL"
    check_result
    if [ $? -eq 1 ]; then
      STATE=$STATE_UNKNOWN
    else
      SUBCMD=$3
      case $SUBCMD in

        act)
          INFO=`grep 'Info' $TEMPFILE | sed -e 's/^Info=.*|Status=\(.*\)|CurrentlyActiveJobs.*$/\1/'`
          check $4 $5 $INFO
          STATE=$?
          echo "$INFO"
          ;;

        jobcount)
          INFO=`grep 'Info' $TEMPFILE | sed -e 's/^Info=.*|CurrentlyActiveJobs=\(.*\)|.*$/\1/'`
          if [ "$INFO" = "" ]; then
            STATE=$STATE_UNKNOWN
          else
            check $4 $5 $INFO
            STATE=$?
            echo "$INFO"
          fi
          ;;

        *)
          STATE=$STATE_UNKNOWN
          echo "Unknown subcommand $SUBCMD"
          ;;

      esac
    fi
    ;;

  msgq)
    URL="$URL&cmd=006&msgqname=$6&msgid=$7&minutes=$8"
    SUBCMD=$3

    case $SUBCMD in

      inq)
        wget -O $TEMPFILE -o $TEMPFILELOG "$URL"
        check_result
        if [ $? -eq 1 ]; then
          STATE=$STATE_UNKNOWN
        else
          INFO=`grep '^Info=.*|Type=05|.*' $TEMPFILE | wc -l`
          check $4 $5 $INFO
          STATE=$?
          echo "$INFO"
        fi
        ;;

      search)
        wget -O $TEMPFILE -o $TEMPFILELOG "$URL"
        check_result
        if [ $? -eq 1 ]; then
          STATE=$STATE_UNKNOWN
        else
          INFO=`grep '^Info=|' $TEMPFILE | wc -l`
          check $4 $5 $INFO
          STATE=$?
          echo "$INFO"
        fi
        ;;

      *)
        STATE=$STATE_UNKNOWN
        echo "Unknown subcommand $SUBCMD"
        ;;

    esac
    ;;

  *)
    STATE=$STATE_UNKNOWN
    echo "Unknown command $CMD"
    ;;

esac

if [ -e $TEMPFILE ]; then
  rm $TEMPFILE
fi

if [ -e $TEMPFILELOG ]; then
  rm $TEMPFILELOG
fi

exit $STATE
