# IBMiNagios

## Features

- API over IBMi Web Service
- No additional IBMi user is needed
- Security
  - API access only from specified IP addresses
  - API access only with valid authorisation key
  - Read only
  - As SQL statement only SELECT is allowed

## Configuration

- IBMi objects
  - /etc/ibminagios.conf
    - rights:
      ```
      -rw------- QTMHHTP1  0
      ```
    - content:
      ```
      AUTHKEY=halala
      ```
  - IBMNAGIOS/IBMNAGIOS *PGM
- Nagios objects
  - /etc/ibminagios.conf
    - rights:
      ```
      -rw------- nagios nobody
      ```
    - content:
      ```
      <host>:AUTHKEY=halala
      <host>:HTTPPORT=81
      ```
  - *<nagios path>*/libexec/checkcommands.conf
    ```
    define command {
      command_name ibminagios
      command_line $USER1$/ibminagios.sh $HOSTADDRESS$ "$ARG1$" "$ARG2$" "$ARG3$" "$ARG4$" "$ARG5$" "$ARG6$" "$ARG7$" "$ARG8$" "$ARG9$"
    }
    ```      
- IBMi Web Service
  - apache.conf
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

## API definition

- Usage
  ```
  https://<host>:<port>/ibminagios?authkey=<authkey>&cmd=<cmd>
  ```
- Commands
  - 001
    - **Required param:** -
    - **Optional param:** -
    - **Output:** Info=|AspUsed=<x>|
  - 002
    - **Required param:** OUTQNAME
    - **Optional param:** OUTQLIB
    - **Output:** **Info=|NumberOfFiles=<x>|
  - 003
    - **Required param:** -
    - **Optional param:** STATUS, JOBNAME
    - **Output:** Info=|JobName=<x>|JobUser=<x>|JobNumber=<x>|Status=<x>|Type=<x>|SubType=<x>|ActiveJobStatus=<x>|SubsystemName=<x>|
  - 004
    - **Required param:** SQL
    - **Optional param:** -
    - **Output:** Info=|Value=<x>|
  - 005
    - **Required param:** -
    - **Optional param:** SBSNAME
    - **Output:** Info=|SubsystemName=<x>|SubsystemLibraryName=<x>|Status=<x>|ActiveJobs=<x>|
  - 006
    - **Required param:** MSGQNAME, MINUTES
    - **Optional param:** MSGID
    - **Output:** Info=|MessageId=<x>|Date=<x>|Time=<x>|Severity=<x>|Type=<x>|

## Example services

- System ASP used
  ```
  check_command ibminagios!syssts!aspused!ge75!ge90
  ```
- Number of spool files in output queue QEZDEBUG
  ```
  check_command ibminagios!outq!splfcount!ge1!-!QEZDEBUG
  ```
- Number of jobs with active status MSGW (with two exceptions)
  ```
  check_command ibminagios!job!msgw!ge1!-!USVJOB,RH_STRZX60
  ```
- Number of active jobs with name USVJOB
  ```
  check_command ibminagios!job!act!-!eq0!USVJOB
  ```
- Number of not active iCluster nodes
  ```
  check_command ibminagios!sql!-!ge1!-!SELECT count(*) AS satz FROM icluster.dmnodes WHERE status <> '*ACTIVE'
  ```