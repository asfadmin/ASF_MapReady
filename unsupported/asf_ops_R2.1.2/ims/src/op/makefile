###############################################################################
#
# File:         makefile
#
# Function:     Make file for the IMS Operater Interface source tree.
#
# Author:       S. Hardman
#
# Date:         4/12/95
#
###############################################################################

include ../makedefs

all:	appl

appl:	
	cd gui_lib; $(MAKE) all
	cd gui_app; $(MAKE) all

clean:
	cd gui_lib; $(MAKE) clean
	cd gui_app; $(MAKE) clean

install:
	cd gui_lib; $(MAKE) install
	cd gui_app; $(MAKE) install
