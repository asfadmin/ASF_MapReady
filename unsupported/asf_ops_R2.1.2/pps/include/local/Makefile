#===============================================================================
# Copyright(c) 1996, California Institute of Technology.
#             ALL RIGHTS RESERVED.
# U.S. Government Sponsorship acknowledged.
#===============================================================================
# Filename:	Makefile
# Description:	Makefile for include/local
# Creator:	Sally Chou
# Notes:	
#
# SCCS Info:
#		@(#)Makefile	1.1  11/21/96
#===============================================================================

SUB_PATH =      include/local

HEADER_FILES = \
		ODLcommonhdr.h		\
		PMF.h			\
		PPSdefs.h		\
		PPSextern.h		\
		PPSexternODL.h		\
		PPShdr.h		\
		dapps_defs.h		\
		dapps_list.h		\
		db_jobs.h		\
		db_l1_order.h		\
		db_l1_procparms.h	\
		db_policy.h		\
		db_scan_order.h		\
		db_sybint.h		\
		defs.h			\
		llist.h			\
		llist_err.h		\
		nmalloc.h		\
		ord_llist.h		\
		ppscount.h		\
		ppsmx.h			\
		timerec.h

default: mkdir llist/Makefile $(HEADER_FILES)
	@for i in llist; \
		do (cd $$i; \
			echo Makefile default in `pwd`; \
                        $(MAKE) default) ; \
		done

source:
	$(MAKE) default

all:
	$(MAKE) clean
	$(MAKE) default

clean: mkdir llist/Makefile
	@for i in llist; \
		do (cd $$i; \
			echo Makefile clean in `pwd`; \
                        $(MAKE) clean) ; \
		done

mkdir:
	if [ ! -d llist ]; \
	then \
		mkdir llist; \
	fi

%/Makefile: $(PPS_TREE)/$(SUB_PATH)/%/SCCS/s.Makefile
	(cd $*; sccs get $(PPS_TREE)/$(SUB_PATH)/$*/SCCS/s.Makefile)

%.h: $(PPS_TREE)/$(SUB_PATH)/SCCS/s.%.h
	sccs get $(PPS_TREE)/$(SUB_PATH)/SCCS/s.$*.h
