###############################################################################
#
# File:         makefile
#
# Function:     Make file for the IMS Media source tree.
#
# Author:       S. Hardman
#
# Date:         7/8/95
#
###############################################################################

include ../makedefs

all:	appl

appl:	
	cd llib; $(MAKE) all
	cd clnt_app; $(MAKE) all

clean:
	cd llib; $(MAKE) clean
	cd clnt_app; $(MAKE) clean

install:
	cd llib; $(MAKE) install
	cd clnt_app; $(MAKE) install
