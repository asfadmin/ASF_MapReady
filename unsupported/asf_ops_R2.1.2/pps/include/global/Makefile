#===============================================================================
# Copyright(c) 1996, California Institute of Technology.
#             ALL RIGHTS RESERVED.
# U.S. Government Sponsorship acknowledged.
#===============================================================================
# Filename:	Makefile
# Description:	Makefile for include/global
# Creator:	Sally Chou
# Notes:	
#
# SCCS Info:
#		@(#)Makefile	1.1  11/21/96
#===============================================================================

SHELL       =   /bin/csh
DELIVER_DIR =   /ASF/$(ASFVER)/include/pps
SUB_PATH    =   include/global

HEADER_FILES = \
		PPS_Extern.h	\
		PPSerr.h	\
		check_status.h

default: $(HEADER_FILES)

source:
	$(MAKE) default

all:
	$(MAKE) clean
	$(MAKE) default

clean:

install:
	(set incs=(*.h); cd $(DELIVER_DIR); /bin/rm -f $$incs)
	/bin/cp -p *.h $(DELIVER_DIR)

%.h: $(PPS_TREE)/$(SUB_PATH)/SCCS/s.%.h
	sccs get $(PPS_TREE)/$(SUB_PATH)/SCCS/s.$*.h
