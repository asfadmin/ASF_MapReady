###############################################################################
#
# File:         Makefile
#
# Function:     Make file for the APS source tree.
#
# Author:       Ron Green (Inspired by S. Hardman)
#
# Date:         02/02/95
#
# SCCS Info:
#	"@(#)Makefile	5.2 98/03/19 APS/ASF"
#	"@(#) /home/aps/r2.1.2/src/SCCS/s.Makefile"
#
###############################################################################

ifdef CM_HOME
	APS_HOME = $(CM_HOME)/aps
endif

include $(APS_HOME)/src/aps_makedefs

VECTOR_LIB = $(APS_HOME)/lib/sparc/global/libdp-vector.a

###############################################################################
#
# The following Local Defines should be moved to aps_makedefs
# if they are ever used by other Makefiles (and renamed as necessary)
#
###############################################################################
# Local Defines the location of the "bin" scripts; needed for DAPPS CM
APS_SH_SCRIPTS	:=	$(APS_HOME)/etc/sh_scripts

# Local Defines the APS top level make related file directory path (Needed 
# to prepare the CMVERSION file for delivery to the CM area)
make_scripts_dirs = ./ 

# Local Defines the APS DB creation script file directory path (Needed 
# to prepare the CMVERSION file for delivery to the CM area)
db_scripts_dirs = ../etc/db/tables ../etc/db/types ../etc/db/reports \
                  ../etc/db/tools/R1_satsensor_recs

# Local Defines the APS shell script file directory path (Needed to prepare the 
# CMVERSION file for delivery to the CM area)
sh_scripts_dirs = ../etc/sh_scripts
install_scripts_dirs = ../etc/install/sh_scripts

# Local Defines the APS include file directory path (Needed to prepare the 
# CMVERSION file for delivery to the CM area)
incl_dirs = ../include/local ../include/global

# Local Defines all the APS file types (Needed to prepare the 
# CMVERSION file for delivery to the CM area)
file_types = *.c *.for *.h *.i *.acf *.idl *.prj *.inc Makefile
###############################################################################
#
# End of Local Defines
#
###############################################################################


#	lib_GENc removed - replaced by lib_interface  

global_lib_dirs =   \
	lib_APS      \
	lib_APSdb    \
	lib_framegen \
	lib_interface \
	lib_llist    \
	lib_phase    \
	lib_sybint   \
	lib_timec

local_lib_dirs =   \
	lib_APSmath \
	lib_APSpmfutils \
	lib_F77APSdb \
	lib_dtkm   \
	lib_fileutils \
	lib_multiUser

lib_dirs = \
	$(global_lib_dirs) \
	$(local_lib_dirs)

# removed:  combined into lib_interface
#	lib_CSAc \
#	lib_ESAc \
#	lib_NASDAc 


appl_dirs = \
    CSA_dtkf_p   \
    CON_roundup  \
	DTK_tools \
	DAR2DTK \
	FA_dtkf_c	\
	FA_dtkf_p	\
    ODL_dtkf_c   \
    ODL_dtkf_p   \
	WOS_tools    \
	asap         \
	ant_roundup  \
	aps_coverage_window \
	aps_encrypt  \
	aps_upd_DAR_stat \
	aps2hc  \
	archive \
	catephm \
	cdtkopps     \
	cnomask      \
	cnomcov      \
	cnomorb      \
	create_files \
	create_sv_files \
	darstats       \
	dtkm_segload \
	framegen       \
	get_sequence_number \
	gui               \
	j1_msgn         \
	mapr          \
	migrate       \
	opln_print   \
	rev2time   \
	stats     \
	stoic     \
	time2rev   \
	write2syslog

###############################################################################
#
# TARGETS
#
###############################################################################

AVAIL_SRC_TARGETS	=	glib glib_inst app bin_inst defs \
						all global_libs local_libs aps \
						apsinstall asfinstall relink \
						clean clean_global_libs clean_local_libs clean_aps \
						cleanapsinstall cleanafsinstall \
						CM_lock CM_unlock CM_info
default : check_env_vars
	@echo
	@echo Available Libraries/Applications for MAKE: ; 
	@for srcdir in $(lib_dirs) $(appl_dirs)  ; do \
	echo "   $$srcdir" ;   \
	done
	@echo
	@echo "Available MAKE targets: $(AVAIL_SRC_TARGETS)
#	@echo "Available MAKE targets: all global_libs local_libs aps" \
#		  "apsinstall asfinstall relink clean cleanapsinstall" \
#		  "CM_lock CM_unlock CM_info" 
	@echo

all : global_libs local_libs aps
	@echo DONE with make $@

global_libs :
	@echo
	@echo Making Global Libraries
	@for srcdir in $(global_lib_dirs) ; \
	do \
	   echo Making $$srcdir ; \
	   cd $$srcdir ;   \
	   $(MAKE) all apsinstall ; \
	   cd .. ;         \
	done

local_libs :
	@echo
	@echo Making Local Libraries
	@for srcdir in $(local_lib_dirs) ; \
	do \
	   echo Making $$srcdir ; \
	   cd $$srcdir ;   \
	   $(MAKE) all apsinstall ; \
	   cd .. ;         \
	done


aps : 
	@echo
	@for srcdir in $(appl_dirs) ; \
	do \
	   echo Making $$srcdir ; \
	   cd $$srcdir ;   \
	   $(MAKE) all apsinstall ; \
	   cd .. ;         \
	done

depend:
	@echo
	@echo Making depend for all src directories
	@for srcdir in $(appl_dirs) $(global_lib_dirs) $(local_lib_dirs); \
	do \
		echo Making depend for $$srcdir ; \
		cd $$srcdir ;   \
		sccs edit Makefile; \
		$(MAKE) depend; \
		echo "Re-run make depend"|sccs delget Makefile; \
		cd .. ;         \
	done

###############################################################################
#
# DAPPS CM Targets
#
###############################################################################
glib :
	$(MAKE) clean_global_libs
	$(MAKE) global_libs

app :
	$(MAKE) clean_local_libs
	$(MAKE) local_libs
	$(MAKE) clean_aps
	$(MAKE) aps

glib_inst :
	@echo
	@echo Removing existing global include files from ASF area
	@ - rm -f $(ASFINC)/aps/*.h
	@echo
	@echo Copying global include files
	@cp -p $(subst -I,,$(IAPS_GLOBAL))/*.h $(ASFINC)/aps
	@echo
	@echo Copying global library files
	@$(MAKE) 'APS_INSTALL_GLOBAL_LIB_DIR=$(ASF_INSTALL_LIB_DIR)' \
			install_global_lib

BIN_SCRIPT_FILES	=	$(shell 'ls' $(APS_SH_SCRIPTS)/aps*) \
						$(APS_SH_SCRIPTS)/echo2
bin_inst :
	@echo Copying Script files: 
	@echo $(BIN_SCRIPT_FILES)
	@echo
	@rm -f $(addprefix $(ASFBIN)/, $(notdir $(BIN_SCRIPT_FILES)))
	@cp -p $(BIN_SCRIPT_FILES) $(ASFBIN)
	@echo Copying Executables
	$(MAKE) 'APS_INSTALL_EXE_DIR=$(ASFBIN)' install_aps
###############################################################################
#
# end of DAPPS CM Targets
#
###############################################################################

install_global_lib :
	@echo
	@for srcdir in $(global_lib_dirs) ; \
	do \
	   cd $$srcdir ;   \
	   $(MAKE) apsinstall ; \
	   cd .. ;         \
	done
	@cp -p $(VECTOR_LIB) $(ASF_INSTALL_LIB_DIR)

install_local_lib:
	@echo
	@for srcdir in $(local_lib_dirs) ; \
	do \
	   cd $$srcdir ;   \
	   $(MAKE) apsinstall ; \
	   cd .. ;         \
	done

install_aps:
	@echo
	@for srcdir in $(appl_dirs) ; \
	do \
	   cd $$srcdir ;   \
	   $(MAKE) apsinstall ; \
	   cd .. ;         \
	done

apsinstall :
	@echo
	@for srcdir in $(lib_dirs) ; \
	do \
	   cd $$srcdir ;   \
	   $(MAKE) apsinstall ; \
	   cd .. ;         \
	done
	@for srcdir in $(appl_dirs) ; \
	do \
	   cd $$srcdir ;   \
	   $(MAKE) apsinstall ; \
	   cd .. ;         \
	done

asfinstall :
	@echo
	@for srcdir in $(appl_dirs) ; \
	do \
	   cd $$srcdir ;   \
	   $(MAKE) all asfinstall ; \
	   cd .. ;         \
	done

relink :
	@echo
	@for srcdir in $(appl_dirs) ; \
	do \
	   cd $$srcdir ;   \
	   $(MAKE) relink apsinstall ; \
	   cd .. ;         \
	done

clean_aps:
	@echo 
	@for srcdir in $(appl_dirs) ; \
	do \
	   cd $$srcdir ;   \
	   $(MAKE) clean ; \
	   cd .. ;         \
	done

clean_global_libs:
	@echo 
	@for srcdir in $(global_lib_dirs) ; \
	do \
	   cd $$srcdir ;   \
	   $(MAKE) clean ; \
	   cd .. ;         \
	done

clean_local_libs:
	@echo 
	@for srcdir in $(local_lib_dirs) ; \
	do \
	   cd $$srcdir ;   \
	   $(MAKE) clean ; \
	   cd .. ;         \
	done

clean cleanapsinstall cleanafsinstall :
	@echo 
	@for srcdir in $(lib_dirs) $(appl_dirs) ; \
	do \
	   cd $$srcdir ;   \
	   $(MAKE) $@ ; \
	   cd .. ;         \
	done

#cleanapsinstall: cleanVERSION

cleanVERSION:
	@for srcdir in $(lib_dirs) ; \
	do \
	   cd $$srcdir ;   \
	   VERSIONfile=`ls VERSION*` ; \
	   cd .. ; \
	   echo rm -f $(APS_INSTALL_LIB_DIR)/$$VERSIONfile ; \
	   rm -f $(APS_INSTALL_LIB_DIR)/$$VERSIONfile ; \
	done
	@for srcdir in $(appl_dirs) ; \
	do \
	   cd $$srcdir ;   \
	   VERSIONfile=`ls VERSION*` ; \
	   cd .. ; \
	   echo rm -f $(APS_INSTALL_EXE_DIR)/$$VERSIONfile ; \
	   rm -f $(APS_INSTALL_EXE_DIR)/$$VERSIONfile ; \
	done

###############################################################################
#
# Create the CMVERSION file for delivery to the CM area. It also lock 
# all files while the delivery proceeds.
# 
###############################################################################
.PHONY:	$(appl_dirs) $(lib_dirs) $(incl_dirs) CM_lock
CM_lock:
	@for srcdir in $(lib_dirs) $(appl_dirs) $(incl_dirs)  ; do \
		$(MAKE) $$srcdir ; \
	done

$(appl_dirs) $(lib_dirs) $(incl_dirs) : 
	@(cd $@; \
	  echo Create the CMVERSION file in `pwd`; \
	  sccs edit $(file_types); \
	  sccs prt -y $(file_types) > CMVERSION; \
	 )
#
# Unlock the files after delivery to the CM area is completed. 
# 
CM_unlock: 
	for srcdir in $(lib_dirs) $(appl_dirs) $(incl_dirs)  ; do \
		(cd $$srcdir; \
	  	echo Unlock files in `pwd`; \
	    sccs unedit $(file_types); \
	 	) \
	done

CM_info: 
	for srcdir in $(lib_dirs) $(appl_dirs) $(incl_dirs) $(sh_scripts_dirs) \
	$(install_scripts_dirs) $(db_scripts_dirs) $(make_scripts_dirs); do \
		(cd $$srcdir; \
	  	echo "check sccs info for files in `pwd`"; \
	    sccs info ; \
	 	) \
	done

###############################################################################
#
# Count the lines of source code in the src and include files
#	count_all: counts all lines in the files
#	count_code: counts lines in files, except blank lines & comment-only lines
#
###############################################################################
COUNT_SRC_DIR		:= $(shell pwd)
SUM_LINES_PROGRAM	:=	$(COUNT_SRC_DIR)/sum_the_lines.pl

COUNT_ALL_PROGRAM	:=	$(COUNT_SRC_DIR)/count_all.csh
COUNT_CODE_PROGRAM	:=	$(COUNT_SRC_DIR)/count_code.pl
COUNT_CODE_FFLAGS	:=	-f
COUNT_CODE_CFLAGS	:=	-c
# counts lines in the files, except blank lines & comment-only lines
count_code :
	@echo
	@echo Counting only lines of code \(not comments or blank lines\)
	@rm -f $(COUNT_SRC_DIR)/count_code_tmp.out
	@for srcdir in $(lib_dirs) $(appl_dirs) $(incl_dirs) ; \
	do \
	   echo "	"Counting lines of code in $$srcdir ; \
	   cd $$srcdir ;   \
	   $(COUNT_CODE_PROGRAM) $(COUNT_CODE_CFLAGS) \
				`(ls *.[ch] 2> /dev/null)` >> \
				$(COUNT_SRC_DIR)/count_code_tmp.out ; \
	   $(COUNT_CODE_PROGRAM) $(COUNT_CODE_FFLAGS) \
				`(ls *.for *.inc 2> /dev/null)` >> \
				$(COUNT_SRC_DIR)/count_code_tmp.out ; \
	   cd $(COUNT_SRC_DIR) ;         \
	done
	@echo TOTAL lines of code: \
		`$(SUM_LINES_PROGRAM) $(COUNT_SRC_DIR)/count_code_tmp.out`
	@rm -f $(COUNT_SRC_DIR)/count_code_tmp.out

# counts all lines in the files
count_all :
	@echo
	@echo Counting Total lines in files
	@rm -f $(COUNT_SRC_DIR)/count_all_tmp.out
	@for srcdir in $(lib_dirs) $(appl_dirs) $(incl_dirs) ; \
	do \
	   echo "	"Counting lines in files in $$srcdir ; \
	   cd $$srcdir ;   \
	   $(COUNT_ALL_PROGRAM) `(ls *.[ch] *.for *.inc 2> /dev/null)` >> \
				$(COUNT_SRC_DIR)/count_all_tmp.out ; \
	   cd $(COUNT_SRC_DIR) ;         \
	done
	@echo TOTAL lines in files: \
		`$(SUM_LINES_PROGRAM) $(COUNT_SRC_DIR)/count_all_tmp.out`
	@rm -f $(COUNT_SRC_DIR)/count_all_tmp.out

$(AVAIL_SRC_TARGETS)	:	check_env_vars

###############################################################################
#
# Check that necessary environment variables are already set
#
###############################################################################
.PHONY	: check_env_vars

check_env_vars :
	ifndef ASFVER
		@echo ""
		@echo \
		'***** Need ASFVER env. variable set *****'
		@echo ""
	endif
	ifndef SYBASE
		@echo ""
		@echo \
		'***** Need SYBASE env. variable set *****'
		@echo ""
	endif
	@# never reach this one because make err exits when setting $(MACH)
	ifndef APS_HOME
		@echo ""
		@echo \
		'***** Need APS_HOME or CM_HOME env. variable set *****'
		@echo ""
	endif

###############################################################################
#
# Print Definitions
#
###############################################################################
defs:
	@echo
	@echo "*****MAKEFILE DEFINITIONS******"
	@echo
	@echo "APS HOME: " $(APS_HOME)
	@echo
	@echo "ASF VER: " $(ASFVER)
	@echo
	@echo "SYBASE: " $(SYBASE)
	@echo
	@echo "ASF INSTALL_EXE_DIR: " $(ASF_INSTALL_EXE_DIR)
	@echo
	@echo "ASF INSTALL_LIB_DIR: " $(ASF_INSTALL_LIB_DIR)
	@echo
	@echo "APS INSTALL_EXE_DIR: " $(APS_INSTALL_EXE_DIR)
	@echo
	@echo "APS INSTALL_LIB_DIR: " $(APS_INSTALL_LIB_DIR)
	@echo
	@echo "APS INSTALL_GLOBAL_LIB_DIR: " $(APS_INSTALL_GLOBAL_LIB_DIR)
	@echo
	@echo "SRC DIR: " $(SRC_DIR)
	@echo
	@echo "TARGETS:  " $(TARGETS)
	@echo
	@echo "F77_LOG:  " $(F77_LOG)
	@echo
	@echo "F77:      " $(F77) 
	@echo
	@echo "F77 SRC:  " $(F77_SRC)
	@echo
	@echo "CC:       " $(CC)
	@echo
	@echo "CFLAGS:   " $(CFLAGS)
	@echo
	@echo "SRC:      " $(SRC)
	@echo
	@echo "INC PATH: " $(IALL)
	@echo
	@echo "LIB PATH: " $(LALL)
	@echo
	@echo "LIBS:     " $(LIBS)
	@echo
	@echo "MACHINE:  " $(MACH)
	@echo
	@echo "OS:       " $(OS)

