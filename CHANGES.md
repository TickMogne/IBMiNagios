# Changes

## 2023.10.04

- bugfix ifs file name
- new shell subcommand
  - ifs emptyfilecount

## 2023.04.16

- new API command 008 (journal receivers)
- new shell command and subcommands
  - jrnrcv partialcount
  - jrnrcv oldestdetached

## 2022.06.24

- changing the parameter *msgq_name* for the command *msgq*
  - adding possibility to specify the library name, example: USERLIB/ERRORQ
  - if the library is not specified, *LIBL will be used (example: QSYSOPR)

## 2022.06.13

- adding the parameter *exception_of_message_ids* for the command *msgq* subcommand *search*

## 2022.06.03

- new API command 007 (IFS directory entries)
- new shell command and subcommands
  - ifs filesize
  - ifs filecount
  - ifs dircount
  - ifs oldestfile


