#===============================================================================
# Copyright(c) 1996, California Institute of Technology.
#             ALL RIGHTS RESERVED.
# U.S. Government Sponsorship acknowledged.
#===============================================================================
# Filename:	Makefile
# Description:	Master makefile for PPS src tree
# Creator:	Norbert Piega
# Notes:	
#
# SCCS Info:
#		@(#)Makefile	1.4  12/19/96
#===============================================================================
default:
	$(MAKE) $(MACH)_default

glib:
	$(MAKE) $(MACH)_glib

source:
	$(MAKE) $(MACH)_source

all:
	$(MAKE) $(MACH)_all

clean:
	$(MAKE) $(MACH)_clean


sparc_default: lib_sybint/Makefile lib_pps/Makefile \
		lib_llist/Makefile lib_mx/Makefile lib_count/Makefile \
		pps_gui/Makefile server/Makefile \
		server_stop/Makefile pps_makedefs lib_extract/Makefile \
		log_browser/Makefile
	(cd lib_extract ; gnumake)
	(cd lib_pps ; gnumake)
	(cd lib_sybint ; gnumake)
	(cd lib_llist ; gnumake)
	(cd lib_mx ; gnumake)
	(cd lib_count ; gnumake)
	(cd pps_gui ; $(MAKE) )
	(cd server ; gnumake)
	(cd server_stop ; gnumake)
	(cd log_browser ; gnumake)

sparc_glib:   pps_makedefs lib_extract/Makefile lib_pps/Makefile
	(cd lib_extract ; gnumake)
	(cd lib_pps ; gnumake)

sparc_source: lib_sybint/Makefile lib_pps/Makefile \
		lib_llist/Makefile lib_mx/Makefile lib_count/Makefile \
		pps_gui/Makefile server/Makefile \
		server_stop/Makefile pps_makedefs lib_extract/Makefile \
		log_browser/Makefile
	(cd lib_extract ; gnumake source)
	(cd lib_pps ; gnumake source)
	(cd lib_sybint ; gnumake source)
	(cd lib_llist ; gnumake source)
	(cd lib_mx ; gnumake source)
	(cd lib_count ; gnumake source)
	(cd pps_gui ; $(MAKE)  source)
	(cd server ; gnumake source)
	(cd server_stop ; gnumake source)
	(cd log_browser ; gnumake source)

sparc_all: lib_sybint/Makefile lib_pps/Makefile \
		lib_llist/Makefile lib_mx/Makefile lib_count/Makefile \
		pps_gui/Makefile server/Makefile \
		server_stop/Makefile pps_makedefs lib_extract/Makefile \
		log_browser/Makefile
	(cd lib_extract ; gnumake all)
	(cd lib_pps ; gnumake all)
	(cd lib_sybint ; gnumake all)
	(cd lib_llist ; gnumake all)
	(cd lib_mx ; gnumake all)
	(cd lib_count ; gnumake all)
	(cd pps_gui ; $(MAKE) all)
	(cd server ; gnumake all)
	(cd server_stop ; gnumake all)
	(cd log_browser ; gnumake all)

sparc_clean: lib_sybint/Makefile lib_pps/Makefile \
		lib_llist/Makefile lib_mx/Makefile lib_count/Makefile \
		pps_gui/Makefile server/Makefile \
		server_stop/Makefile pps_makedefs lib_extract/Makefile \
		log_browser/Makefile
	(cd lib_extract ; gnumake clean)
	(cd lib_pps ; gnumake clean)
	(cd lib_sybint ; gnumake clean)
	(cd lib_llist ; gnumake clean)
	(cd lib_mx ; gnumake clean)
	(cd lib_count ; gnumake clean)
	(cd pps_gui ; $(MAKE) clean)
	(cd server ; gnumake clean)
	(cd server_stop ; gnumake clean)
	(cd log_browser ; gnumake clean)


sgi_default: log_browser/Makefile
	(cd log_browser ; gnumake)

sgi_glib:

sgi_source: log_browser/Makefile
	(cd log_browser ; gnumake source)

sgi_all: log_browser/Makefile
	(cd log_browser ; gnumake all)

sgi_clean: log_browser/Makefile
	(cd log_browser ; gnumake clean)


%/Makefile: $(PPS_TREE)/src/%/SCCS/s.Makefile
	(cd $*; sccs get $(PPS_TREE)/src/$*/SCCS/s.Makefile)

pps_makedefs:
	sccs get $(PPS_TREE)/src/SCCS/s.$@
