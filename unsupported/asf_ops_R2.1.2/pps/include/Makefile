#===============================================================================
# Copyright(c) 1996, California Institute of Technology.
#             ALL RIGHTS RESERVED.
# U.S. Government Sponsorship acknowledged.
#===============================================================================
# Filename:	Makefile
# Description:	makefile for PPS include tree
# Creator:	Sally Chou
# Notes:	
#
# SCCS Info:
#		@(#)Makefile	1.1  11/21/96
#===============================================================================

default: global/Makefile local/Makefile
	(cd global ; $(MAKE))
	(cd local ; $(MAKE))

glib:	global/Makefile
	(cd global ; $(MAKE))

source:
	$(MAKE) default

all:	global/Makefile local/Makefile
	(cd global ; $(MAKE) all)
	(cd local ; $(MAKE) all)

install: global/Makefile
	(cd global; $(MAKE) install)

%/Makefile: $(PPS_TREE)/include/%/SCCS/s.Makefile
	(cd $*; sccs get $(PPS_TREE)/include/$*/SCCS/s.Makefile)
