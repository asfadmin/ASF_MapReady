###############################################################################
#
# File:         makefile
#
# Function:     Make file for the IMS FA source tree.
#
# Author:       S. Hardman, D. Crichton
#
# Date:         1/31/96
#
###############################################################################

include ../makedefs

all:	appl

appl:	
	cd mlib; $(MAKE) all
	cd llib; $(MAKE) all
	cd clnt_app; $(MAKE) all
	cd gui_app; $(MAKE) all

clean:
	cd mlib; $(MAKE) clean
	cd llib; $(MAKE) clean
	cd clnt_app; $(MAKE) clean 
	cd gui_app; $(MAKE) clean

install:
	cd mlib; $(MAKE) install
	cd llib; $(MAKE) install
	cd clnt_app; $(MAKE) install
	cd gui_app; $(MAKE) install

