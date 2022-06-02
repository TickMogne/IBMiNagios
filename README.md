# IBMiNagios

## Features

- API over IBMi Web Service
- No additional IBMi user is needed
- Security
  - API access only from specified IP addresses
  - API access only with valid authorisation key
  - Read only
  - As SQL statement only SELECT is allowed
- The Nagios check command is the script *ibminagios.sh*, this script calls the IBMi Web Service (IBMi program *ibminagios*) with authorisation key, command and command parameters and processes the output of the API.

## Configuration

- IBMi objects
  - /etc/ibminagios.conf
    - rights (only the user *QTMHHTP1* should able to read and write the file):
      ```
      -rw------- QTMHHTP1  0
      ```
    - content:
      ```
      AUTHKEY=halala
      ```
      - Authorisation key to use the API.
  - IBMNAGIOS/IBMNAGIOS *PGM
- Nagios objects
  - the tool *wget* must be installed
  - /etc/ibminagios.conf
    - rights (only the user *nagios* should able to read and write the file):
      ```
      -rw------- nagios nobody
      ```
    - content:
      ```
      <host>:AUTHKEY=halala
      <host>:HTTPPORT=81
      ```
      - Authorisation key and API port fpr each monitored hosts.
  - *&lt;nagios path&gt;*/etc/checkcommands.conf
    ```
    define command {
      command_name ibminagios
      command_line $USER1$/ibminagios.sh $HOSTADDRESS$ "$ARG1$" "$ARG2$" "$ARG3$" "$ARG4$" "$ARG5$" "$ARG6$" "$ARG7$" "$ARG8$" "$ARG9$"
    }
    ```
  - *&lt;nagios path&gt;*/libexec/ibminagios.sh
- IBMi Web Service
  - httpd.conf
    ```
    ScriptAlias /ibminagios /QSYS.LIB/IBMINAGIOS.LIB/IBMINAGIOS.PGM
    <Directory /QSYS.LIB/IBMINAGIOS.LIB/>
      Order Deny,Allow
      Allow From 10.43.85.172
      Deny From all
      SetHandler cgi-script
      Options +ExecCGI
      SetEnv IBMI_NAGIOS_CONF_FILE /etc/ibminagios.conf
    </Directory>
    ```
    - you should change the IP address to the address of the nagios (only nagios will be able use this API).
    - you may use the SetEnv IBMI_NAGIOS_CONF_FILE to define the path of the ibminagios.conf file on the IBMi. The default path is /etc/ibminagios.conf.

## ibminagios.sh

```
Usage:
  ibminagios.sh <host> <command>
    command: [ syssts aspused <warning_condition> <critical_condition> ] |
             [ job [ msgw <warning_condition> <critical_condition> <exception_of_job_names> ] |
             [ act <warning_condition> <critical_condition> <job_name> ] ] |
             [ outq splfcount <warning_condition> <critical_condition> <outq_name> ] |
             [ sql - <warning_condition> <critical_condition> <sql_statement> ] |
             [ sbs [ act <warning_condition> <critical_condition> <sbs_name> ] |
                   [ jobcount <warning_condition> <critical_condition> <sbs_name> ] ] |
             [ msgq [ inq <warning_condition> <critical_condition> <msgq_name> <msgid | *ALL> <minutes> ] |
                    [ search <warning_condition> <critical_condition> <msgq_name> <msgid | *ALL> <minutes> ] ]
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
      special value: <NULL>
    exception_of_job_names: Job names separated with comma
    exception_of_msgid: Job names separated with comma
```

## ibminagios

- Usage
  ```
  https://<host>:<port>/ibminagios?authkey=<auth_key>&cmd=<command>
  ```
- Commands
  - 001
    - **Required param:** -
    - **Optional param:** -
    - **Output:** Info=|AspUsed=&lt;x&gt;|
  - 002
    - **Required param:** OUTQNAME
    - **Optional param:** OUTQLIB
    - **Output:** Info=|NumberOfFiles=&lt;x&gt;|
  - 003
    - **Required param:** -
    - **Optional param:** STATUS, JOBNAME
    - **Output:** Info=|JobName=&lt;x&gt;|JobUser=&lt;x&gt;|JobNumber=&lt;x&gt;|Status=&lt;x&gt;|Type=&lt;x&gt;|SubType=&lt;x&gt;|ActiveJobStatus=&lt;x&gt;|SubsystemName=&lt;x&gt;|
  - 004
    - **Required param:** SQL
    - **Optional param:** -
    - **Output:** Info=|Value=&lt;x&gt;|
  - 005
    - **Required param:** -
    - **Optional param:** SBSNAME
    - **Output:** Info=|SubsystemName=&lt;x&gt;|SubsystemLibraryName=&lt;x&gt;|Status=&lt;x&gt;|ActiveJobs=&lt;x&gt;|
  - 006
    - **Required param:** MSGQNAME, MINUTES
    - **Optional param:** MSGID
    - **Output:** Info=|MessageId=&lt;x&gt;|Date=&lt;x&gt;|Time=&lt;x&gt;|Severity=&lt;x&gt;|Type=&lt;x&gt;|

## Example services

- System ASP used
  Warning from 75%, critical from 90%.
  ```
  check_command ibminagios!syssts!aspused!ge75!ge90
  ```
- Number of spool files in output queue QEZDEBUG
  Warning if there is at least 1 spool file in the queue, no critical check.
  ```
  check_command ibminagios!outq!splfcount!ge1!-!QEZDEBUG
  ```
- Number of jobs with active status MSGW (with two exceptions)
  Warning if there is at least 1 job has active status MSGW, nor critical check. The job names USVJOB and RH_STRZX60 are exceptions (they have always active status MSGW).
  ```
  check_command ibminagios!job!msgw!ge1!-!USVJOB,RH_STRZX60
  ```
- Number of active jobs with name USVJOB
  No warning check, critical if the number of of jobs is 0 (the job is not active).
  ```
  check_command ibminagios!job!act!-!eq0!USVJOB
  ```
- Number of not active iCluster nodes
  Warning if the number of not active nodes greater equal 1, no critical check. 
  ```
  check_command ibminagios!sql!-!ge1!-!SELECT count(*) AS satz FROM icluster.dmnodes WHERE status <> '*ACTIVE'
  ```
- Number of jobs in the subsystem ZEITSBSD
  Warning if the number of jobs is less than 7, no critical check.
  ```
  check_command ibminagios!sbs!jobcount!lt7!-!ZEITSBSD
  ```
- Status of the OMK subsystem
  No warning check, critical if the status of the subsystem is not *ACTIVE
  ```
  check_command ibminagios!sbs!act!-!ne*ACTIVE!OMK
  ```
- Number of messages in the message QSECOFR in the last 15 minutes
  Warning if the number of messages in the last 15 minutes is greater than 0, no critical check.
  ```
  check_command ibminagios!msgq!search!gt0!-!QSECOFR!*ALL!15
  ```
