#!/bin/csh -f

source $HOME/set_env.csh

$FAIF_BINPATH/NASDAdirmon \
        -c $FAIF_ROOTPATH/NASDA/config/NASDAdirmon.config

exit 0
