#!/bin/csh -f

setenv ASFVER    R1BP

setenv FAIF_BINPATH  /ASF/$ASFVER"/bin"
setenv FAIF_ROOTPATH /LOCAL/$ASFVER"/faif"
setenv APS_DATA       /LOCAL/$ASFVER"/aps"

#Sybase related.
setenv SYBASE ~sybase/syb1002
setenv DSQUERY DAPPSDB

# Sybase online documentation
#setenv SYBROOT ~sybase/sybooks
#setenv EBTRC   $SYBROOT/sun5m/.ebtrc
#set path = ($SYBROOT/sun5m/bin $path)

exit 0

