###############################################################################
#
# File:         Makefile
#
# Function:     Baseline Version of APS Makefile
#
# Author:       Ron Green (Inspired by S. Hardman)
#
# Date:         02/02/95
#
# SCCS Info:
#	"@(#)Makefile	5.1 98/01/08 APS/ASF"
#	"@(#) /home/aps/r2.1.2/src/lib_APSdb/SCCS/s.Makefile"
#
###############################################################################

ifdef CM_HOME
	APS_HOME = $(CM_HOME)/aps
endif

include $(APS_HOME)/src/aps_makedefs


###############################################################################
#
# Source Default Directory
#   Typically off of $APS_HOME/src... this is used to retrieve source
#
###############################################################################
 
SRC_DIR = $(APS_HOME)/src/lib_APSdb
 

###############################################################################
#
# Include Path Definitions
#    Delete lines that contain items you don't need
#    Always use the OSIFLAGS
#
###############################################################################

IALL = \
	$(IAPS) \
	$(ISYBASE) \
	$(OSIFLAGS) 

###############################################################################
#
# Library Path Definitions
#    Delete lines that contain items you don't need
#    Always use the OSLFLAGS
#
###############################################################################

LALL = 

RALL = $(foreach Llib, $(LALL), $(subst -L, -R, $(Llib)))

###############################################################################
#
# Library Definitions
#    Delete lines that contain items you don't need
#    Always use the OSLIBS
#    If a line has two libraries the first is dependent on the second
#
#    The STATIC and DYNAMIC macro can be used to set the link option for
#    the libraries that follow them
#    The 3rd Party and system libs are defaulted to link dynamically 
#    (see aps_makedefs)
#    The MATH_LIB for some reason works best when it's last
#
###############################################################################

LIBS = 

###############################################################################
#
# Flags and Defines
#
###############################################################################

DEFS =      -D$(MACH)
CFLAGS =    $(OSCFLAGS) $(DEFS)

###############################################################################
#
# Source and Target Definition
#   Add your source and exe filenames here
#       EXE      is the name of the executable to be created
#       LIB      is the name of the library to be created
#       SRC      is a list of the C source code files
#       HEADER_FILES is a list of the C Header files
#       F77_SRC  is a list (possibly empty) of the Fortran 77 source
#                code files
#       F77_INC  is a list (possibly empty) of the Fortran 77 include files
#
#
###############################################################################

EXE          =  

LIB          = 

GLOBAL_LIB 	 =  libapsdb.a

SRC          =       \
				aps_db_table.c   \
				aps_db_table_utils.c   \
				aps_get_sequence_number.c  \
				aps_new_dtkid_errors.c    \
				db_get_new_dtkid.c    

HEADER_FILES =  

F77_SRC      =  

F77_INC      =  

OBJS         = $(SRC:%.c=%.o) $(F77_SRC:%.for=%.o)

TARGETS = $(EXE) $(LIB) $(GLOBAL_LIB)
 

###############################################################################
#
# Make the Application
#    Add all targets to be made to the next line
#    If more exes or libs are needed make a copy of the set of 
#    needed commands
#
###############################################################################

all appl:   $(TARGETS)

$(EXE): $(OBJS)
	@echo
	@echo "Linking    $(EXE)"
	@$(CC) -o $(EXE) $(OBJS) $(LALL) $(RALL) $(LIBS)
	@echo "Done"
 
$(LIB): $(OBJS)
	@echo
	@echo "Creating/Modifying Library $(LIB)"
	@$(ARCHIVE) rucv $(LIB) $?
	@if [ $(OS) = sunos ]; then ranlib $(LIB); fi
	@echo "Done"

$(GLOBAL_LIB): $(OBJS)
	@echo
	@echo "Creating/Modifying Library $(GLOBAL_LIB)"
	@$(ARCHIVE) rucv $(GLOBAL_LIB) $?
	@if [ $(OS) = sunos ]; then ranlib $(GLOBAL_LIB); fi
	@echo "Done"


###############################################################################
#
# Source Code Retrieval
#
###############################################################################

$(SRC) $(HEADER_FILES) $(F77_SRC) $(F77_INC) :
	@echo
	@echo "Retrieving $@ as of $(FILE_VERSION) "
	@echo $(CM) $(CM_GET) $(FILE_VERSION) $(SRC_DIR)/$@
	@$(CM) $(CM_GET) $(FILE_VERSION) $(SRC_DIR)/$@

 
###############################################################################
#
# C Compilation.
#
###############################################################################

%.o:%.c
	@echo "Compiling $<"
	@$(CC) $(CFLAGS) $(IALL) -c $<


###############################################################################
#
# Fortran Compilation.
#
###############################################################################

%.o:%.for $(F77_INC)
	@echo
	@echo "Compiling $<"
	@$(F77_LOG) ; $(F77) $<


###############################################################################
#
# Object and executable cleaning.
#
###############################################################################

clean:
	@echo
	@echo "Cleaning " $(TARGETS)
	rm -f $(OBJS) $(TARGETS)
	@echo

###############################################################################
#
# Lint
#
###############################################################################

lint:
	$(LINT) $(IALL) $(SRC)


###############################################################################
#
# List definitions, install target:
#
###############################################################################

include $(APS_HOME)/src/aps_post_makedefs

###############################################################################
#
# Makedepend (create all the include file dependencies
#	(destroys everything after the "DO NOT DELETE ..." line)
#
###############################################################################
depend: $(SRC) $(F77_SRC)
	@if [ -w Makefile ] ; \
	then \
		echo makedepend -- $(CFLAGS) -- \
			$(IALL) -I$(APS_HOME)/src/makedepend_files $^; \
		makedepend -- $(CFLAGS) -- \
			$(IALL) -I$(APS_HOME)/src/makedepend_files $^; \
		echo rm -f Makefile.bak ; \
		rm -f Makefile.bak ; \
	else \
		echo "*** Unwriteable Makefile: Need to check it out? ***" ; \
	fi

# DO NOT DELETE THIS LINE -- make depend depends on it.

aps_db_table.o: /home/aps/r2.1.1/include/global/db_sybint.h
aps_db_table.o: /home/sybase/sol/include/sybfront.h
aps_db_table.o: /home/sybase/sol/include/cstypes.h
aps_db_table.o: /home/sybase/sol/include/csconfig.h
aps_db_table.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
aps_db_table.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
aps_db_table.o: /usr/include/sys/feature_tests.h
aps_db_table.o: /home/sybase/sol/include/syblogin.h
aps_db_table.o: /home/sybase/sol/include/syberror.h
aps_db_table.o: /home/aps/r2.1.1/include/global/dapps_list.h
aps_db_table.o: /home/aps/r2.1.1/include/global/llist_err.h
aps_db_table.o: /home/aps/r2.1.1/include/global/llist.h
aps_db_table.o: /home/aps/r2.1.1/include/global/ord_llist.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_activ_conf.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_active_dar_activities.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_active_planning_activities.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_active_single_activities.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_activities.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_antenna.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_antenna_down_times.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_antenna_pref.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_antenna_priority.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_csa_data.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_cvrg.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_cvrgmdat.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_dar.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_dl2obs.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_dtk.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_dtkid_lock.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_ephemeris.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_framegen_calls.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_j1_dn_times.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_maskinout.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_maskinout_mdat.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_multi_user_wait.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_permission_counter.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_phase.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_rgs_down_times.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_sat_inclusion_period.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_sat_rev_dtkid.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_satsensor.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_schstat.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_schunavail.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_seg.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_site.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_sscvrg.h
aps_db_table.o: /home/aps/r2.1.1/include/local/db_station.h
aps_db_table_utils.o: /usr/include/stdio.h /usr/include/sys/feature_tests.h
aps_db_table_utils.o: /usr/include/stdlib.h /usr/include/string.h
aps_db_table_utils.o: /usr/include/values.h
aps_db_table_utils.o: /home/aps/r2.1.1/include/global/dapps_defs.h
aps_db_table_utils.o: /home/aps/r2.1.1/include/global/db_sybint.h
aps_db_table_utils.o: /home/sybase/sol/include/sybfront.h
aps_db_table_utils.o: /home/sybase/sol/include/cstypes.h
aps_db_table_utils.o: /home/sybase/sol/include/csconfig.h
aps_db_table_utils.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
aps_db_table_utils.o: /home/sybase/sol/include/sybdb.h
aps_db_table_utils.o: /home/sybase/sol/include/syblogin.h
aps_db_table_utils.o: /home/sybase/sol/include/syberror.h
aps_db_table_utils.o: /home/aps/r2.1.1/include/global/dapps_list.h
aps_db_table_utils.o: /home/aps/r2.1.1/include/global/llist_err.h
aps_db_table_utils.o: /home/aps/r2.1.1/include/global/llist.h
aps_db_table_utils.o: /home/aps/r2.1.1/include/global/ord_llist.h
aps_db_table_utils.o: /home/aps/r2.1.1/include/local/aps_db_table.h
aps_db_table_utils.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
aps_get_sequence_number.o: /home/aps/r2.1.1/include/global/aps_get_sequence_number.h
aps_get_sequence_number.o: /home/aps/r2.1.1/include/global/db_sybint.h
aps_get_sequence_number.o: /home/sybase/sol/include/sybfront.h
aps_get_sequence_number.o: /home/sybase/sol/include/cstypes.h
aps_get_sequence_number.o: /home/sybase/sol/include/csconfig.h
aps_get_sequence_number.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
aps_get_sequence_number.o: /home/sybase/sol/include/sybdb.h
aps_get_sequence_number.o: /usr/include/stdio.h
aps_get_sequence_number.o: /usr/include/sys/feature_tests.h
aps_get_sequence_number.o: /home/sybase/sol/include/syblogin.h
aps_get_sequence_number.o: /home/sybase/sol/include/syberror.h
aps_get_sequence_number.o: /home/aps/r2.1.1/include/global/dapps_list.h
aps_get_sequence_number.o: /home/aps/r2.1.1/include/global/llist_err.h
aps_get_sequence_number.o: /home/aps/r2.1.1/include/global/llist.h
aps_get_sequence_number.o: /home/aps/r2.1.1/include/global/ord_llist.h
aps_get_sequence_number.o: /usr/include/stdlib.h
db_get_new_dtkid.o: /usr/include/string.h /usr/include/sys/feature_tests.h
db_get_new_dtkid.o: /usr/include/stdlib.h
db_get_new_dtkid.o: /home/aps/r2.1.1/include/global/db_sybint.h
db_get_new_dtkid.o: /home/sybase/sol/include/sybfront.h
db_get_new_dtkid.o: /home/sybase/sol/include/cstypes.h
db_get_new_dtkid.o: /home/sybase/sol/include/csconfig.h
db_get_new_dtkid.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
db_get_new_dtkid.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
db_get_new_dtkid.o: /home/sybase/sol/include/syblogin.h
db_get_new_dtkid.o: /home/sybase/sol/include/syberror.h
db_get_new_dtkid.o: /home/aps/r2.1.1/include/global/dapps_list.h
db_get_new_dtkid.o: /home/aps/r2.1.1/include/global/llist_err.h
db_get_new_dtkid.o: /home/aps/r2.1.1/include/global/llist.h
db_get_new_dtkid.o: /home/aps/r2.1.1/include/global/ord_llist.h
db_get_new_dtkid.o: /home/aps/r2.1.1/include/global/nmalloc.h
db_get_new_dtkid.o: /home/aps/r2.1.1/include/global/aps_get_sequence_number.h
