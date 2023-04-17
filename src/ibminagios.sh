#!/bin/sh

#
# ibminagios.sh
#
# Created by: github.com/TickMogne, 2022.05.31
#

#
# usage()
#
# Parameters:
#  -
#
# Return value:
#  -
#
usage() {
  cat <<EOF
ibminagios.sh <host> <command>
  command: [ syssts aspused <warning_condition> <critical_condition> ] |
           [ job [ msgw <warning_condition> <critical_condition> <exception_of_job_names> ] |
                 [ act <warning_condition> <critical_condition> <job_name> ] ] |
           [ outq splfcount <warning_condition> <critical_condition> <outq_name> ] |
           [ sql - <warning_condition> <critical_condition> <sql_statement> ] |
           [ sbs [ act <warning_condition> <critical_condition> <sbs_name> ] |
                 [ jobcount <warning_condition> <critical_condition> <sbs_name> ] ] |
           [ msgq [ inq <warning_condition> <critical_condition> <msgq_name> <msgid | *ALL> <minutes> ] |
                  [ search <warning_condition> <critical_condition> <msgq_name> <msgid | *ALL> <minutes> [ <exception_of_message_ids> ] ] ] |
           [ ifs [ filecount <warning_condition> <critical_condition> <dir_name> ] |
                 [ dircount <warning_condition> <critical_condition> <dir_name> ] |
                 [ filesize <warning_condition> <critical_condition> <dir_name> <file_name> ] |
                 [ oldestfile <warning_condition> <critical_condition> <dir_name> ] ]
           [ jrnrcv [ partialcount <warning_condition> <critical_condition> ] |
                    [ oldestdetached <warning_condition> <critical_condition> [ <journal> ] ] ]
  warning_condition and critical_condition:
    format: [ <condition><value> | - ]
      -  : no condition processing
    conditions:
      lt : less than 
      le : less equal 
      eq : equal
      ne : not equal 
      ge : greater equal
      gt : greater than
    values (per command and subcommand):
      syssts aspused        : percent of usage
      job msgw              : number of jobs
      job act               : number of jobs
      outq splfcount        : number of spool files
      sql                   : return value of the sql statement
      sbs act               : status of the subsystem
      sbs jobcount          : number of jobs
      msgq inq              : number of messages
      msgq search           : number of messages
      ifs filecount         : number of files
      ifs dircount          : number of directories
      ifs filesize          : size of the file in bytes
      ifs oldestfile        : age of the file in seconds
      jrnrcv partialcount   : number of journal receivers
      jrnrcv oldestdetached : age of the journal receiver (datached date and time) in seconds
    special value: <NULL>
  exception_of_job_names: Job names separated with comma
  exception_of_message_ids: Message ids separated with comma
EOF
}

#
# check_condition()
#
# Parameters:
#  1. condition
#  2. value to check
#
# Return value:
#  Returns 1 if the result is true, otherwise returns 0.
#
check_condition() {
  local A
  local T

  if [ "$1" = "-" ]; then
    return 0
  fi

  VALUE=`echo $1 | sed -e 's/^[a-z][a-z]\(.*\)/\1/'`
  T=`echo $VALUE | grep '^[0-9]*$'`
  if [ "$T" = "" ]; then
    T="STRING"
  else
    T="NUMBER"
  fi

  A=`echo $1 | grep '^lt'` 
  if [ "$A" != "" ]; then
    VALUE=`echo $1 | sed -e 's/^lt\(.*\)$/\1/'`
    case $T in
      NUMBER)
        if [ $2 -lt $VALUE ]; then
          return 1
        fi
        ;;
      STRING)
        if [[ "$2" < "$VALUE" ]]; then
          return 1
        fi
        ;;
    esac
    return 0
  fi

  A=`echo $1 | grep '^le'` 
  if [ "$A" != "" ]; then
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

  A=`echo $1 | grep '^eq'` 
  if [ "$A" != "" ]; then
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

  A=`echo $1 | grep '^ne'` 
  if [ "$A" != "" ]; then
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

  A=`echo $1 | grep '^ge'` 
  if [ "$A" != "" ]; then
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

  A=`echo $1 | grep '^gt'` 
  if [ "$A" != "" ]; then
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

#
# check_conditions()
#
# Parameters:
#  1. critical condition
#  2. warning condition
#  3. value to check
#
# Return value:
#  Returns the nagios status (OK, WARNING, CRITICAL).
#
check_conditions() {
  # check CIRITAL first
  check_condition $2 $3
  RET=$?

  # if CRITICAL
  if [ $RET -eq 1 ]; then
    return $STATE_CRITICAL
  else
    # check WARNING
    check_condition $1 $3
    RET=$?

    # if WARNING
    if [ $RET -eq 1 ]; then
      return $STATE_WARNING
    fi
  fi;

  return $STATE_OK
}

#
# check_api_result()
#
# Parameters:
#  -
#
# Return value:
#  Returns 1 if the result is ok, otherwise returns 0.
#
check_api_result() {
  ERROR=`grep ' ERROR [0-9]*: ' $TEMPFILELOG`
  if [ "$ERROR" != "" ]; then
    echo $ERROR
    return 0
  fi

  RESULT=`grep 'Result=' $TEMPFILE | sed -e 's/^Result=\(.*\)\r$/\1/'`
  if [ "$RESULT" != "OK" ]; then
    INFO=`grep 'Info' $TEMPFILE | sed -e 's/^Info=\(.*\)\r$/\1/'`
    echo $INFO
    return 0
  fi

  return 1
}

#
# check_exception()
#
# Parameters:
#  1. List of the exceptions, delimited with comma
#  2. Text to check in the list of exceptions
#
# Return value:
#  Returns 1 if the text in the list of exceptions, otherwise return 0.
#
check_exception() {
  local EXCEPTIONS
  local INARRAY

  IFS=',' read -ra EXCEPTIONS <<< "$1"

  INARRAY=$(echo " ${EXCEPTIONS[@]} " | grep -o " $2 " | wc -w)
  if [ $INARRAY -eq 0 ]; then
    return 0
  else
    return 1
  fi
}

#
# main
#
if [ "$1" = "" ]; then
  usage
  exit 0
fi

PROGPATH=$(echo "$0" | sed -e 's,[\\/][^\\/][^\\/]*$,,')
. "$PROGPATH"/utils.sh

CONFIGFILE=/etc/ibminagios.conf

HOST=$1
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

case $CMD in

  syssts)
    URL="$URL&cmd=001"
    wget -O $TEMPFILE -o $TEMPFILELOG "$URL"
    check_api_result
    if [ $? -eq 0 ]; then
      STATE=$STATE_UNKNOWN
    else
      SUBCMD=$3

      case $SUBCMD in

        aspused)
          INFO=`grep 'Info' $TEMPFILE | sed -e 's/^Info=*.|AspUsed=\([0-9]*\).*$/\1/'`
          if [ "$INFO" = "" ]; then
            STATE=$STATE_UNKNOWN
          else
            check_conditions $4 $5 $INFO
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
    check_api_result
    if [ $? -eq 0 ]; then
      exit $STATE_UNKNOWN
    else

      case $SUBCMD in

        msgw)
          C=0
          while read LINE
          do
            INFO=`echo $LINE | grep '^Info=|'`
            if [ "$INFO" != "" ]; then
              ACTIVEJOBSTATUS=`echo $INFO | sed -e 's/^Info=.*|ActiveJobStatus=\(.*\)|SubsystemName=.*$/\1/'`
              if [ "$ACTIVEJOBSTATUS" = "MSGW" ]; then
                JOBNAME=`echo $INFO | sed -e 's/^Info=.*|JobName=\(.*\)|JobUser=.*$/\1/'`
                if [ "$6" = "" ]; then
                  let C=C+1
                else
                  check_exception $6 $JOBNAME
                  if [ $? -eq 0 ]; then
                    let C=C+1
                  fi
                fi
              fi
            fi
          done < $TEMPFILE
          check_conditions $4 $5 $C
          STATE=$?
          echo -n $C
          if [ "$6" != "" ]; then
            echo ", exceptions: $6"
          else
            echo ""
          fi
          ;;

        act)
          INFO=`grep '^Info=|' $TEMPFILE | wc -l`
          check_conditions $4 $5 $INFO
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
    check_api_result
    if [ $? -eq 0 ]; then
      STATE=$STATE_UNKNOWN
    else
      SUBCMD=$3
      case $SUBCMD in

        splfcount)
          INFO=`grep 'Info' $TEMPFILE | sed -e 's/^Info=.*|NumberOfFiles=\([0-9]*\).*$/\1/'`
          if [ "$INFO" = "" ]; then
            STATE=$STATE_UNKNOWN
          else
            check_conditions $4 $5 $INFO
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
    check_api_result
    if [ $? -eq 0 ]; then
      STATE=$STATE_UNKNOWN
    else
      INFO=`grep 'Info' $TEMPFILE | sed -e 's/^Info=.*|Value=\(.*\)|.*$/\1/'`
      check_conditions $4 $5 $INFO
      STATE=$?
      echo "$INFO"
    fi
    ;;

  sbs)
    URL="$URL&cmd=005&sbsname=$6"
    wget -O $TEMPFILE -o $TEMPFILELOG "$URL"
    check_api_result
    if [ $? -eq 0 ]; then
      STATE=$STATE_UNKNOWN
    else
      SUBCMD=$3
      case $SUBCMD in

        act)
          INFO=`grep 'Info' $TEMPFILE | sed -e 's/^Info=.*|Status=\(.*\)|CurrentlyActiveJobs.*$/\1/'`
          check_conditions $4 $5 $INFO
          STATE=$?
          echo "$INFO"
          ;;

        jobcount)
          INFO=`grep 'Info' $TEMPFILE | sed -e 's/^Info=.*|CurrentlyActiveJobs=\([0-9]*\).*$/\1/'`
          if [ "$INFO" = "" ]; then
            STATE=$STATE_UNKNOWN
            echo "No Information"
          else
            check_conditions $4 $5 $INFO
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
        check_api_result
        if [ $? -eq 0 ]; then
          STATE=$STATE_UNKNOWN
        else
          INFO=`grep '^Info=.*|Type=05|.*' $TEMPFILE | wc -l`
          check_conditions $4 $5 $INFO
          STATE=$?
          echo "$INFO"
        fi
        ;;

      search)
        wget -O $TEMPFILE -o $TEMPFILELOG "$URL"
        check_api_result
        if [ $? -eq 0 ]; then
          STATE=$STATE_UNKNOWN
        else
          C=0
          while read LINE
          do
            INFO=`echo $LINE | grep '^Info=|'`
            if [ "$INFO" != "" ]; then
              MESSAGEID=`echo $INFO | sed -e 's/^Info=.*|MessageId=\(.*\)|Date=.*$/\1/'`
              check_exception $9 $MESSAGEID
              if [ $? -eq 0 ]; then
                let C=C+1
              fi
            fi
          done < $TEMPFILE
          check_conditions $4 $5 $C
          STATE=$?
          echo -n $C
          if [ "$9" != "" ]; then
            echo ", exceptions: $9"
          else
            echo ""
          fi
        fi
        ;;

      *)
        STATE=$STATE_UNKNOWN
        echo "Unknown subcommand $SUBCMD"
        ;;

    esac
    ;;

  ifs)
    URL="$URL&cmd=007&dir=$6"
    SUBCMD=$3

    case $SUBCMD in

      filecount)
        wget -O $TEMPFILE -o $TEMPFILELOG "$URL"
        check_api_result
        if [ $? -eq 0 ]; then
          STATE=$STATE_UNKNOWN
        else
          INFO=`grep '^Info=.*|Type=FILE|.*$' $TEMPFILE | wc -l`
          check_conditions $4 $5 $INFO
          STATE=$?
          echo "$INFO"
        fi
        ;;

      dircount)
        wget -O $TEMPFILE -o $TEMPFILELOG "$URL"
        check_api_result
        if [ $? -eq 0 ]; then
          STATE=$STATE_UNKNOWN
        else
          INFO=`grep '^Info=.*|Type=DIR|.*$' $TEMPFILE | wc -l`
          check_conditions $4 $5 $INFO
          STATE=$?
          echo "$INFO"
        fi
        ;;

      filesize)
        URL="$URL&file=$7"
        wget -O $TEMPFILE -o $TEMPFILELOG "$URL"
        check_api_result
        if [ $? -eq 0 ]; then
          STATE=$STATE_UNKNOWN
        else
          INFO=`grep 'Info' $TEMPFILE | sed -e 's/^Info=.*|Size=\([0-9]*\).*$/\1/'`
          if [ "$INFO" = "" ]; then
            STATE=$STATE_UNKNOWN
          else
            check_conditions $4 $5 $INFO
            STATE=$?
            echo "$INFO bytes"
          fi
        fi
        ;;

      oldestfile)
        URL="$URL&file=$7"
        wget -O $TEMPFILE -o $TEMPFILELOG "$URL"
        check_api_result
        if [ $? -eq 0 ]; then
          STATE=$STATE_UNKNOWN
        else
          AGE=0
          DATETIME=0
          FILE=""
          while read LINE
          do
            INFO=`echo $LINE | grep '^Info=|.*|Type=FILE|.*'`
            if [ "$INFO" != "" ]; then
              MODIFIEDDATETIME=`echo $INFO | sed -e 's/^Info=.*|ModifiedDateTime=\([0-9]*\).*$/\1/'`
              YYYYMMDD=${MODIFIEDDATETIME:0:8}
              HH=${MODIFIEDDATETIME:8:2}
              MM=${MODIFIEDDATETIME:10:2}
              SS=${MODIFIEDDATETIME:12:2}
              let T=($(date +%s)-$(date +%s -d $YYYYMMDD)-$HH*3600-$MM*60-$SS)
              if [ $T -gt $AGE ]; then
                AGE=$T
                let DATETIME=$(date +%s -d $YYYYMMDD)+$HH*3600+$MM*60+$SS
                FILE=`echo $INFO | sed -e 's/^Info=.*|Name=\(.*\)|Type=.*$/\1/'`
              fi
            fi
          done < $TEMPFILE
          if [ $AGE -gt 0 ]; then
            INFO=$AGE
            check_conditions $4 $5 $INFO
            STATE=$?
            INFO=`date --date=@$DATETIME +"%Y.%m.%d %H:%M:%S"`
            echo "$INFO $FILE"
          else
            echo "-"
          fi 
        fi
        ;;

      *)
        STATE=$STATE_UNKNOWN
        echo "Unknown subcommand $SUBCMD"
        ;;

    esac
    ;;

  jrnrcv)
    URL="$URL&cmd=008"
    SUBCMD=$3

    case $SUBCMD in

      partialcount)
        wget -O $TEMPFILE -o $TEMPFILELOG "$URL"
        check_api_result
        if [ $? -eq 0 ]; then
          STATE=$STATE_UNKNOWN
        else
          INFO=`grep '^Info=.*|Status=5|.*$' $TEMPFILE | wc -l`
          check_conditions $4 $5 $INFO
          STATE=$?
          if [ $INFO -gt 0 ]; then
            INFO2=`grep '^Info=.*|Status=5|.*$' $TEMPFILE | sed -e 's/^.*|JrnRcvName=\(.*\)|JrnRcvLibrary.*$/\1/'`
            INFO="$INFO, $INFO2"
          fi
          echo "$INFO"
        fi
        ;;

      oldestdetached)
        wget -O $TEMPFILE -o $TEMPFILELOG "$URL"
        check_api_result
        if [ $? -eq 0 ]; then
          STATE=$STATE_UNKNOWN
        else
          if [ -z "$6" ]; then
            JRNLIBRARY=""
            JRNNAME=""
          else
            if [ `echo $6 | grep -l '/' | wc -l` -eq 0 ]; then
              JRNLIBRARY=""
              JRNNAME=$6
            else
              JRNLIBRARY=`echo "$6" | sed -e 's/^\(.*\)[/].*$/\1/'`
              JRNNAME=`echo "$6" | sed -e 's/^.*[/]\(.*\)$/\1/'`
            fi
          fi
          AGE=0
          DATETIME=0
          FILE=""
          LIBRARY=""
          while read LINE
          do
            if [ -z "$JRNLIBRARY" ] && [ -z "$JRNNAME" ]; then
              INFO=`echo $LINE | grep '^Info=|.*|Status=[234]|.*'`
            elif [ -z "$JRNLIBRARY" ] && [ -n "$JRNNAME" ]; then
              C="^Info=|.*|JrnName=$JRNNAME|.*|Status=[234]|.*"
              INFO=`echo $LINE | grep "$C"`
            else
              C="^Info=|.*|JrnName=$JRNNAME|JrnLibrary=$JRNLIBRARY|Status=[234]|.*"
              INFO=`echo $LINE | grep "$C"`
            fi
            if [ "$INFO" != "" ]; then
              MODIFIEDDATETIME=`echo $INFO | sed -e 's/^Info=.*|DetachedDateTime=\([0-9]*\).*$/\1/'`
              YYYYMMDD=${MODIFIEDDATETIME:0:8}
              HH=${MODIFIEDDATETIME:8:2}
              MM=${MODIFIEDDATETIME:10:2}
              SS=${MODIFIEDDATETIME:12:2}
              let T=($(date +%s)-$(date +%s -d $YYYYMMDD)-$HH*3600-$MM*60-$SS)
              if [ $T -gt $AGE ]; then
                AGE=$T
                let DATETIME=$(date +%s -d $YYYYMMDD)+$HH*3600+$MM*60+$SS
                FILE=`echo $INFO | sed -e 's/^Info=.*|JrnRcvName=\(.*\)|JrnRcvLibrary=.*$/\1/'`
                LIBRARY=`echo $INFO | sed -e 's/^Info=.*|JrnRcvLibrary=\(.*\)|JrnName=.*$/\1/'`
              fi
            fi
          done < $TEMPFILE
          if [ $AGE -gt 0 ]; then
            INFO=$AGE
            check_conditions $4 $5 $INFO
            STATE=$?
            INFO=`date --date=@$DATETIME +"%Y.%m.%d %H:%M:%S"`
            echo "$INFO $LIBRARY/$FILE"
          else
            echo "-"
          fi
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
