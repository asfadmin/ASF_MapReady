###############################################################################
#
# File:         makefile
#
# Function:     Make file for the IMS V0 source tree.
#
# Author:       S. Hardman
#
# Date:         11/18/94
#
###############################################################################

include ../makedefs

all:	glib_ext appl

glib_ext:
	cd llib; $(MAKE) all

appl:	
	cd clnt_app; $(MAKE) all
	cd srv_app; $(MAKE) all

clean:
	cd llib; $(MAKE) clean
	cd clnt_app; $(MAKE) clean
	cd srv_app; $(MAKE) clean

install:
	cd clnt_app; $(MAKE) install
	cd srv_app; $(MAKE) install
