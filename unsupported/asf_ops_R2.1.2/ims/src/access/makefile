###############################################################################
#
# File:         makefile
#
# Function:     Make file for the IMS Access source tree.
#
# Author:       S. Hardman
#
# Date:         5/2/96
#
###############################################################################

include ../makedefs

all:	appl

appl:	
	cd clnt_app; $(MAKE) all
	cd gui_app; $(MAKE) all
	cd telnetd_app; $(MAKE) all

clean:
	cd clnt_app; $(MAKE) clean
	cd gui_app; $(MAKE) clean
	cd telnetd_app; $(MAKE) clean

install:
	cd clnt_app; $(MAKE) install
	cd gui_app; $(MAKE) install
	cd telnetd_app; $(MAKE) install
