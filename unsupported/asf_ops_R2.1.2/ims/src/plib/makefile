###############################################################################
#
# File:         makefile
#
# Function:     Make file for the Public Library source tree.
#
# Author:       S. Hardman
#
# Date:         1/11/96
#
###############################################################################

include ../makedefs

all:	glib_ext glib_int

glib_ext:
	cd ik; $(MAKE) all
	cd odl; $(MAKE) all

glib_int:	
	cd hdf; $(MAKE) all
	cd odl_ik; $(MAKE) all

clean:
	cd hdf; $(MAKE) clean
	cd ik; $(MAKE) clean
	cd odl; $(MAKE) clean
	cd odl_ik; $(MAKE) clean

ext_inst:
	cd ik; $(MAKE) install
	cd odl; $(MAKE) install

int_inst:
	cd hdf; $(MAKE) install
	cd odl_ik; $(MAKE) install
