###############################################################################
#
# File:         makefile
#
# Function:     Make file for the IMS FTS source tree.
#
# Author:       S. Hardman
#
# Date:         9/22/94
#
###############################################################################

include ../makedefs

all:	appl

appl:	
	cd llib; $(MAKE) all
	cd clnt_app; $(MAKE) all
	cd srv_app; $(MAKE) all

clean:
	cd llib; $(MAKE) clean
	cd clnt_app; $(MAKE) clean
	cd srv_app; $(MAKE) clean

install:
	cd llib; $(MAKE) install
	cd clnt_app; $(MAKE) install
	cd srv_app; $(MAKE) install
