#!/bin/csh -f

source $HOME/set_env.csh

# Remove TMDF files for now
#mv $FAIF_ROOTPATH/ADEOS/TMDF* $FAIF_ROOTPATH/ADEOS/TMDF_hold/ >&! /dev/null
rm $FAIF_ROOTPATH/ADEOS/TMDF* >&! /dev/null

$FAIF_BINPATH/ADEOSdirmon \
        -c $FAIF_ROOTPATH/ADEOS/config/ADEOSdirmon.config

exit 0
