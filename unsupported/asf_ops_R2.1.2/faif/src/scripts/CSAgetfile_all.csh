#!/bin/csh -f

# The "ping" section of code was added at suggestion of Gary Walker.

# Start by "pinging" the CSA host. If there is already a connection,
# proceed at once. Otherwise, wait two minutes before proceeding. This
# allows a connection to be established and hopefully avoids an ftp
# timeout.

source $HOME/set_env.csh

set node_tmp = `grep CSA_SRCHOST $FAIF_ROOTPATH/CSA/config/CSAgetfile.config`
set node = $node_tmp[2]
set alive = `/usr/sbin/ping $node`
if ("$alive" != $node" is alive") sleep 120

# Set the location of the executables.

source $HOME/set_env.csh

# Call CSAgetfile for each file type.

$FAIF_BINPATH/CSAgetfile   -f CSA_DEFVORBIT  -c $FAIF_ROOTPATH/CSA/config/CSAgetfile.config
$FAIF_BINPATH/CSAgetfile   -f CSA_PREDORBIT  -c $FAIF_ROOTPATH/CSA/config/CSAgetfile.config

$FAIF_BINPATH/mdel_ufdrf_orb.csh $node $FAIF_ROOTPATH/FTP/upw/CSAufdrfF.upw mcs_to_ufdrf_f

$FAIF_BINPATH/CSAgetfile   -f CSA_RECRQST    -c $FAIF_ROOTPATH/CSA/config/CSAgetfile.config
$FAIF_BINPATH/CSAgetfile   -f CSA_RECSCHED   -c $FAIF_ROOTPATH/CSA/config/CSAgetfile.config
$FAIF_BINPATH/CSAgetfile   -f CSA_SARPROCPRM -c $FAIF_ROOTPATH/CSA/config/CSAgetfile.config

$FAIF_BINPATH/mdel_ufdrf_orb.csh $node $FAIF_ROOTPATH/FTP/upw/CSAufdrfM.upw mcs_to_ufdrf_m

$FAIF_BINPATH/CSAgetfile   -f CSA_RRQ_MCM    -c $FAIF_ROOTPATH/CSA/config/CSAgetfile.config
$FAIF_BINPATH/CSAgetfile   -f CSA_RSH_MCM    -c $FAIF_ROOTPATH/CSA/config/CSAgetfile.config

# Put the size listing of the remotelisting files into the syslog.

/bin/ls -to $FAIF_ROOTPATH/FTP/gtmp/remotelisting.20? | \
/usr/ucb/logger -p local6.debug -t $FAIF_BINPATH"/CSAgetfile" -i

exit 0
