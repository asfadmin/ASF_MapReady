#!/bin/csh -f

source ~faif/set_env.csh

$FAIF_BINPATH/WALPSdirmon  -c $FAIF_ROOTPATH/WALPS/config/WALPSdirmon.config

exit 0
