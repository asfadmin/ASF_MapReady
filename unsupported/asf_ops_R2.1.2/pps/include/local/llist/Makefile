#===============================================================================
# Filename:	Makefile
# Description:	Makefile for include/local/llist
# Creator:	Sally Chou
# Notes:	
#
# SCCS Info:
#		@(#)Makefile	1.1  11/21/96
#===============================================================================

SUB_PATH = include/local/llist
HEADER_FILES = \
		dapps_defs.h	\
		dapps_list.h	\
		llist.h		\
		llist_err.h	\
		nmalloc.h	\
		ord_llist.h

default: $(HEADER_FILES)

source:
	$(MAKE) default

all:
	$(MAKE) clean
	$(MAKE) default

clean:

%.h: $(PPS_TREE)/$(SUB_PATH)/SCCS/s.%.h
	sccs get $(PPS_TREE)/$(SUB_PATH)/SCCS/s.$*.h
