###############################################################################
#
# File:         makefile
#
# Function:     Make file for the IMS Dispatcher source tree.
#
# Author:       S. Hardman, D. Crichton
#
# Date:         1/31/96
#
###############################################################################

include ../makedefs

all:	appl

appl:	
	cd llib; $(MAKE) all
	cd srv_app; $(MAKE) all

clean:
	cd llib; $(MAKE) clean
	cd srv_app; $(MAKE) clean 

install:
	cd llib; $(MAKE) install
	cd srv_app; $(MAKE) install

