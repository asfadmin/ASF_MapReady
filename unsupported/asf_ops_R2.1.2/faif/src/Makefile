#===============================================================================
# Filename:	Makefile
# Description:	Master Makefile
# Creator:	Norbert Piega (Based on makefiles by RGreen and JGrieggs)
# Notes:	
# 	This is the top-level makefile for a subsystem.  Libraries get 
# built first, then executables, and finally test stuff.
# 1.  March '96
#     Replaced unwanted spaces by tabs
#     Explicitly specify gnumake for certain directories
#
# SCCS Info:
#  Makefile
#  1
#  20 Mar 1996
#  @(#)Makefile	1.4
#===============================================================================

glib:
	cd ASF; $(MAKE) clean all
	cd CSAparse; $(MAKE) clean all
	cd util; $(MAKE) clean all
	cd xlate; $(MAKE) clean all
glib_inst:
	cp ../lib/sparc/global/libfasf.a          /ASF/$(ASFVER)/lib/sparc
	cp ../lib/sparc/global/libcsap.a          /ASF/$(ASFVER)/lib/sparc
	cp ../lib/sparc/global/libutil.a          /ASF/$(ASFVER)/lib/sparc
	cp ../lib/sparc/global/libxlat.a          /ASF/$(ASFVER)/lib/sparc
	-cp -r ../include/local/*                 /ASF/$(ASFVER)/include/faif
	-cp -r ../include/global/*                /ASF/$(ASFVER)/include/faif

bin_inst:
	cp ../bin/sparc/*                        /ASF/$(ASFVER)/bin

app: all
all:
	cd scripts; $(MAKE) all
	cd util; $(MAKE) all
	cd xlate; $(MAKE) all
#	cd llist; $(MAKE) all
	cd FAutils; $(MAKE) all
	cd ASF; $(MAKE) all
	cd ESA; $(MAKE) all
	cd NASDA; $(MAKE) all
	cd CSA; $(MAKE) all
	cd WALPS; $(MAKE) all
	cd ADEOS; $(MAKE) all
	cd CSAparse; $(MAKE) all
	cd xlatCSAsv; $(MAKE) all
	cd idfile; $(MAKE) all
	cd idCSAfilename; $(MAKE) all
	cd dirmon_common; $(MAKE) all
	cd PMF; $(MAKE) all
	cd ADEOSproc; $(MAKE) all
	cd ADEOSmsg; $(MAKE) all
	cd ADEOSrcpt; $(MAKE) all
	cd sema; $(MAKE) all
	cd asf2fa; $(MAKE) new_uuid all
	cd CSAPredOrRes; $(MAKE) all
#	cd xlatWALPSdt; $(MAKE) all
	cd FAparse; $(MAKE) all
	cd ESA2PMF; $(MAKE) all
	cd NASDA2PMF; $(MAKE) all
	cd CSA2PMF; $(MAKE) all
	cd WALPS2PMF; $(MAKE) all
	cd ADEOS2PMF; $(MAKE) all
	cd ims_archive_FAIF_file; $(MAKE) all
	cd encapps; $(MAKE) all
	cd router; mkall
	cd CSAgetfile; $(MAKE) all
	cd par; $(MAKE) all
	cd dirmonMain; mkall
#	cd remove_version; $(MAKE) all

clean:
	cd scripts; $(MAKE) clean
	cd util; $(MAKE) clean
	cd xlate; $(MAKE) clean
#	cd llist; $(MAKE) clean
	cd FAutils; $(MAKE) clean
	cd ASF; $(MAKE) clean
	cd ESA; $(MAKE) clean
	cd NASDA; $(MAKE) clean
	cd CSA; $(MAKE) clean
	cd WALPS; $(MAKE) clean
	cd ADEOS; $(MAKE) clean
	cd CSAparse; $(MAKE) clean
	cd idfile; $(MAKE) clean
	cd idCSAfilename; $(MAKE) clean
	cd PMF; $(MAKE) clean
	cd ADEOSproc; $(MAKE) clean
	cd ADEOSmsg; $(MAKE) clean
	cd ADEOSrcpt; $(MAKE) clean
	cd asf2fa; $(MAKE) clean
	cd xlatCSAsv; $(MAKE) clean
	cd CSAPredOrRes; $(MAKE) clean
#	cd xlatWALPSdt; $(MAKE) clean
	cd FAparse; $(MAKE) clean
	cd ESA2PMF; $(MAKE) clean
	cd NASDA2PMF; $(MAKE) clean
	cd CSA2PMF; $(MAKE) clean
	cd WALPS2PMF; $(MAKE) clean
	cd ADEOS2PMF; $(MAKE) clean
	cd ims_archive_FAIF_file; $(MAKE) clean
	cd sema; $(MAKE) clean
	cd dirmon_common; $(MAKE) clean
	cd encapps; $(MAKE) clean
	cd router; gnumake DM=ESA_DM clean
	cd router; gnumake DM=NASDA_DM clean
	cd router; gnumake DM=CSA_DM clean
	cd router; gnumake DM=WALPS_DM clean
	cd router; gnumake DM=ADEOS_DM clean
	cd CSAgetfile; $(MAKE) clean
	cd par; $(MAKE) clean
#	cd remove_version; $(MAKE) clean
	cd dirmonMain; gnumake DM=ESA_DM clean
	cd dirmonMain; gnumake DM=NASDA_DM clean
	cd dirmonMain; gnumake DM=WALPS_DM clean
	cd dirmonMain; gnumake DM=ADEOS_DM clean
