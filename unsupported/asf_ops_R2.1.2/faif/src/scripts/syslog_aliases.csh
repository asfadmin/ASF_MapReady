#!/bin/csh -f

set SYS_LOG_NAME = `egrep -e "^local6\.debug" /etc/syslog.conf | cut -c 14-80`

alias null_log cp /dev/null $SYS_LOG_NAME
alias tail_log tail -30 $SYS_LOG_NAME
alias fail_log "tail -f  $SYS_LOG_NAME"
alias ed_log "/bin/cp $SYS_LOG_NAME FAIFsyslog.log; emacs FAIFsyslog.log; /bin/rm *FAIFsyslog.log* &"

exit 0
