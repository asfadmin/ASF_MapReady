###############################################################################
#
# File:         lib_phase/Makefile 
#
# Function:     APS Makefile for libmps.a
#
# Author:       Ron Green (Inspired by S. Hardman)
#
# Date:         02/02/95
#
# SCCS Info:
#	"@(#)Makefile	5.1 98/01/08 APS/ASF"
#	"@(#) /home/aps/r2.1.2/src/lib_phase/SCCS/s.Makefile"
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
 
SRC_DIR = $(APS_HOME)/src/lib_phase
 

###############################################################################
#
# Include Path Definitions
#    Delete lines that contain items you don't need
#    Always use the OSIFLAGS
#
###############################################################################

IALL = \
	$(IAPS) \
	$(IXMOTIF) \
	$(IGKS) \
	$(ISYBASE) \
	$(IQAPARTNER) \
	$(IENCRYPT) \
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

GLOBAL_LIB          =  libphase.a

SRC          =  \
				asftime_2_phase.c           \
				asftime2rev.c               \
				check_rev_asftimes.c        \
				check_rev_times_overlap.c   \
				decode_error_message.c      \
				phase_asftime2rev.c         \
				phase_rev_in_phase.c        \
				phase_decode_error.c        \
				phase_first_rev.c           \
				phase_get_list.c            \
				phase_rev2asftime.c         \
				phase_rev2et.c              \
				rev2asftime.c               \
				rev_2_phase.c 

HEADER_FILES =  

F77_SRC      =  

F77_INC      =  

OBJS         = $(SRC:%.c=%.o) $(F77_SRC:%.for=%.o)

TARGETS = $(LIB) $(EXE) $(GLOBAL_LIB)
#  TARGETS = $(EXE) $(LIB) ${EXE}.tc
 

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
	@echo
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

asftime_2_phase.o: /usr/include/string.h /usr/include/sys/feature_tests.h
asftime_2_phase.o: /home/aps/r2.1.1/include/global/db_sybint.h
asftime_2_phase.o: /home/sybase/sol/include/sybfront.h
asftime_2_phase.o: /home/sybase/sol/include/cstypes.h
asftime_2_phase.o: /home/sybase/sol/include/csconfig.h
asftime_2_phase.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
asftime_2_phase.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
asftime_2_phase.o: /home/sybase/sol/include/syblogin.h
asftime_2_phase.o: /home/sybase/sol/include/syberror.h
asftime_2_phase.o: /home/aps/r2.1.1/include/global/dapps_list.h
asftime_2_phase.o: /home/aps/r2.1.1/include/global/llist_err.h
asftime_2_phase.o: /home/aps/r2.1.1/include/global/llist.h
asftime_2_phase.o: /home/aps/r2.1.1/include/global/ord_llist.h
asftime_2_phase.o: /home/aps/r2.1.1/include/local/aps_db_table.h
asftime_2_phase.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
asftime_2_phase.o: /home/aps/r2.1.1/include/local/phase_utilities.h
asftime_2_phase.o: /home/aps/r2.1.1/include/local/db_phase.h
asftime_2_phase.o: /home/aps/r2.1.1/include/global/dapps_defs.h
asftime_2_phase.o: /home/aps/r2.1.1/include/local/timeconv.h
asftime2rev.o: /home/aps/r2.1.1/include/local/phase_utilities.h
asftime2rev.o: /home/aps/r2.1.1/include/global/db_sybint.h
asftime2rev.o: /home/sybase/sol/include/sybfront.h
asftime2rev.o: /home/sybase/sol/include/cstypes.h
asftime2rev.o: /home/sybase/sol/include/csconfig.h
asftime2rev.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
asftime2rev.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
asftime2rev.o: /usr/include/sys/feature_tests.h
asftime2rev.o: /home/sybase/sol/include/syblogin.h
asftime2rev.o: /home/sybase/sol/include/syberror.h
asftime2rev.o: /home/aps/r2.1.1/include/global/dapps_list.h
asftime2rev.o: /home/aps/r2.1.1/include/global/llist_err.h
asftime2rev.o: /home/aps/r2.1.1/include/global/llist.h
asftime2rev.o: /home/aps/r2.1.1/include/global/ord_llist.h
asftime2rev.o: /home/aps/r2.1.1/include/local/db_phase.h
asftime2rev.o: /home/aps/r2.1.1/include/global/dapps_defs.h
asftime2rev.o: /home/aps/r2.1.1/include/local/timeconv.h
asftime2rev.o: /usr/include/string.h
check_rev_asftimes.o: /usr/include/string.h /usr/include/sys/feature_tests.h
check_rev_asftimes.o: /usr/include/stdio.h
check_rev_asftimes.o: /home/aps/r2.1.1/include/global/db_sybint.h
check_rev_asftimes.o: /home/sybase/sol/include/sybfront.h
check_rev_asftimes.o: /home/sybase/sol/include/cstypes.h
check_rev_asftimes.o: /home/sybase/sol/include/csconfig.h
check_rev_asftimes.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
check_rev_asftimes.o: /home/sybase/sol/include/sybdb.h
check_rev_asftimes.o: /home/sybase/sol/include/syblogin.h
check_rev_asftimes.o: /home/sybase/sol/include/syberror.h
check_rev_asftimes.o: /home/aps/r2.1.1/include/global/dapps_list.h
check_rev_asftimes.o: /home/aps/r2.1.1/include/global/llist_err.h
check_rev_asftimes.o: /home/aps/r2.1.1/include/global/llist.h
check_rev_asftimes.o: /home/aps/r2.1.1/include/global/ord_llist.h
check_rev_asftimes.o: /home/aps/r2.1.1/include/local/aps_db_table.h
check_rev_asftimes.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
check_rev_asftimes.o: /home/aps/r2.1.1/include/local/db_dtk.h
check_rev_asftimes.o: /home/aps/r2.1.1/include/local/db_phase.h
check_rev_asftimes.o: /home/aps/r2.1.1/include/local/timeconv.h
check_rev_asftimes.o: /home/aps/r2.1.1/include/local/phase_utilities.h
check_rev_asftimes.o: /home/aps/r2.1.1/include/global/dapps_defs.h
check_rev_times_overlap.o: /usr/include/string.h
check_rev_times_overlap.o: /usr/include/sys/feature_tests.h
check_rev_times_overlap.o: /usr/include/stdio.h
check_rev_times_overlap.o: /home/aps/r2.1.1/include/global/db_sybint.h
check_rev_times_overlap.o: /home/sybase/sol/include/sybfront.h
check_rev_times_overlap.o: /home/sybase/sol/include/cstypes.h
check_rev_times_overlap.o: /home/sybase/sol/include/csconfig.h
check_rev_times_overlap.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
check_rev_times_overlap.o: /home/sybase/sol/include/sybdb.h
check_rev_times_overlap.o: /home/sybase/sol/include/syblogin.h
check_rev_times_overlap.o: /home/sybase/sol/include/syberror.h
check_rev_times_overlap.o: /home/aps/r2.1.1/include/global/dapps_list.h
check_rev_times_overlap.o: /home/aps/r2.1.1/include/global/llist_err.h
check_rev_times_overlap.o: /home/aps/r2.1.1/include/global/llist.h
check_rev_times_overlap.o: /home/aps/r2.1.1/include/global/ord_llist.h
check_rev_times_overlap.o: /home/aps/r2.1.1/include/local/aps_db_table.h
check_rev_times_overlap.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
check_rev_times_overlap.o: /home/aps/r2.1.1/include/local/db_dtk.h
check_rev_times_overlap.o: /home/aps/r2.1.1/include/local/db_phase.h
check_rev_times_overlap.o: /home/aps/r2.1.1/include/local/timeconv.h
check_rev_times_overlap.o: /home/aps/r2.1.1/include/local/phase_utilities.h
check_rev_times_overlap.o: /home/aps/r2.1.1/include/global/dapps_defs.h
decode_error_message.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
decode_error_message.o: /home/aps/r2.1.1/include/global/db_sybint.h
decode_error_message.o: /home/sybase/sol/include/sybfront.h
decode_error_message.o: /home/sybase/sol/include/cstypes.h
decode_error_message.o: /home/sybase/sol/include/csconfig.h
decode_error_message.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
decode_error_message.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
decode_error_message.o: /usr/include/sys/feature_tests.h
decode_error_message.o: /home/sybase/sol/include/syblogin.h
decode_error_message.o: /home/sybase/sol/include/syberror.h
decode_error_message.o: /home/aps/r2.1.1/include/global/dapps_list.h
decode_error_message.o: /home/aps/r2.1.1/include/global/llist_err.h
decode_error_message.o: /home/aps/r2.1.1/include/global/llist.h
decode_error_message.o: /home/aps/r2.1.1/include/global/ord_llist.h
decode_error_message.o: /home/aps/r2.1.1/include/local/phase_utilities.h
decode_error_message.o: /home/aps/r2.1.1/include/local/db_phase.h
decode_error_message.o: /home/aps/r2.1.1/include/global/dapps_defs.h
decode_error_message.o: /usr/include/string.h
phase_asftime2rev.o: /home/aps/r2.1.1/include/local/phase_utilities.h
phase_asftime2rev.o: /home/aps/r2.1.1/include/global/db_sybint.h
phase_asftime2rev.o: /home/sybase/sol/include/sybfront.h
phase_asftime2rev.o: /home/sybase/sol/include/cstypes.h
phase_asftime2rev.o: /home/sybase/sol/include/csconfig.h
phase_asftime2rev.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
phase_asftime2rev.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
phase_asftime2rev.o: /usr/include/sys/feature_tests.h
phase_asftime2rev.o: /home/sybase/sol/include/syblogin.h
phase_asftime2rev.o: /home/sybase/sol/include/syberror.h
phase_asftime2rev.o: /home/aps/r2.1.1/include/global/dapps_list.h
phase_asftime2rev.o: /home/aps/r2.1.1/include/global/llist_err.h
phase_asftime2rev.o: /home/aps/r2.1.1/include/global/llist.h
phase_asftime2rev.o: /home/aps/r2.1.1/include/global/ord_llist.h
phase_asftime2rev.o: /home/aps/r2.1.1/include/local/db_phase.h
phase_asftime2rev.o: /home/aps/r2.1.1/include/global/dapps_defs.h
phase_asftime2rev.o: /home/aps/r2.1.1/include/local/timeconv.h
phase_rev_in_phase.o: /home/aps/r2.1.1/include/local/phase_utilities.h
phase_rev_in_phase.o: /home/aps/r2.1.1/include/global/db_sybint.h
phase_rev_in_phase.o: /home/sybase/sol/include/sybfront.h
phase_rev_in_phase.o: /home/sybase/sol/include/cstypes.h
phase_rev_in_phase.o: /home/sybase/sol/include/csconfig.h
phase_rev_in_phase.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
phase_rev_in_phase.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
phase_rev_in_phase.o: /usr/include/sys/feature_tests.h
phase_rev_in_phase.o: /home/sybase/sol/include/syblogin.h
phase_rev_in_phase.o: /home/sybase/sol/include/syberror.h
phase_rev_in_phase.o: /home/aps/r2.1.1/include/global/dapps_list.h
phase_rev_in_phase.o: /home/aps/r2.1.1/include/global/llist_err.h
phase_rev_in_phase.o: /home/aps/r2.1.1/include/global/llist.h
phase_rev_in_phase.o: /home/aps/r2.1.1/include/global/ord_llist.h
phase_rev_in_phase.o: /home/aps/r2.1.1/include/local/db_phase.h
phase_rev_in_phase.o: /home/aps/r2.1.1/include/global/dapps_defs.h
phase_first_rev.o: /home/aps/r2.1.1/include/local/phase_utilities.h
phase_first_rev.o: /home/aps/r2.1.1/include/global/db_sybint.h
phase_first_rev.o: /home/sybase/sol/include/sybfront.h
phase_first_rev.o: /home/sybase/sol/include/cstypes.h
phase_first_rev.o: /home/sybase/sol/include/csconfig.h
phase_first_rev.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
phase_first_rev.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
phase_first_rev.o: /usr/include/sys/feature_tests.h
phase_first_rev.o: /home/sybase/sol/include/syblogin.h
phase_first_rev.o: /home/sybase/sol/include/syberror.h
phase_first_rev.o: /home/aps/r2.1.1/include/global/dapps_list.h
phase_first_rev.o: /home/aps/r2.1.1/include/global/llist_err.h
phase_first_rev.o: /home/aps/r2.1.1/include/global/llist.h
phase_first_rev.o: /home/aps/r2.1.1/include/global/ord_llist.h
phase_first_rev.o: /home/aps/r2.1.1/include/local/db_phase.h
phase_first_rev.o: /home/aps/r2.1.1/include/global/dapps_defs.h
phase_get_list.o: /home/aps/r2.1.1/include/global/db_sybint.h
phase_get_list.o: /home/sybase/sol/include/sybfront.h
phase_get_list.o: /home/sybase/sol/include/cstypes.h
phase_get_list.o: /home/sybase/sol/include/csconfig.h
phase_get_list.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
phase_get_list.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
phase_get_list.o: /usr/include/sys/feature_tests.h
phase_get_list.o: /home/sybase/sol/include/syblogin.h
phase_get_list.o: /home/sybase/sol/include/syberror.h
phase_get_list.o: /home/aps/r2.1.1/include/global/dapps_list.h
phase_get_list.o: /home/aps/r2.1.1/include/global/llist_err.h
phase_get_list.o: /home/aps/r2.1.1/include/global/llist.h
phase_get_list.o: /home/aps/r2.1.1/include/global/ord_llist.h
phase_get_list.o: /home/aps/r2.1.1/include/local/aps_db_table.h
phase_get_list.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
phase_get_list.o: /home/aps/r2.1.1/include/global/dapps_defs.h
phase_get_list.o: /home/aps/r2.1.1/include/local/phase_utilities.h
phase_get_list.o: /home/aps/r2.1.1/include/local/db_phase.h
phase_get_list.o: /home/aps/r2.1.1/include/local/timeconv.h
phase_rev2asftime.o: /home/aps/r2.1.1/include/local/phase_utilities.h
phase_rev2asftime.o: /home/aps/r2.1.1/include/global/db_sybint.h
phase_rev2asftime.o: /home/sybase/sol/include/sybfront.h
phase_rev2asftime.o: /home/sybase/sol/include/cstypes.h
phase_rev2asftime.o: /home/sybase/sol/include/csconfig.h
phase_rev2asftime.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
phase_rev2asftime.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
phase_rev2asftime.o: /usr/include/sys/feature_tests.h
phase_rev2asftime.o: /home/sybase/sol/include/syblogin.h
phase_rev2asftime.o: /home/sybase/sol/include/syberror.h
phase_rev2asftime.o: /home/aps/r2.1.1/include/global/dapps_list.h
phase_rev2asftime.o: /home/aps/r2.1.1/include/global/llist_err.h
phase_rev2asftime.o: /home/aps/r2.1.1/include/global/llist.h
phase_rev2asftime.o: /home/aps/r2.1.1/include/global/ord_llist.h
phase_rev2asftime.o: /home/aps/r2.1.1/include/local/db_phase.h
phase_rev2asftime.o: /home/aps/r2.1.1/include/global/dapps_defs.h
phase_rev2asftime.o: /home/aps/r2.1.1/include/local/timeconv.h
phase_rev2asftime.o: /usr/include/string.h
phase_rev2et.o: /home/aps/r2.1.1/include/local/phase_utilities.h
phase_rev2et.o: /home/aps/r2.1.1/include/global/db_sybint.h
phase_rev2et.o: /home/sybase/sol/include/sybfront.h
phase_rev2et.o: /home/sybase/sol/include/cstypes.h
phase_rev2et.o: /home/sybase/sol/include/csconfig.h
phase_rev2et.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
phase_rev2et.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
phase_rev2et.o: /usr/include/sys/feature_tests.h
phase_rev2et.o: /home/sybase/sol/include/syblogin.h
phase_rev2et.o: /home/sybase/sol/include/syberror.h
phase_rev2et.o: /home/aps/r2.1.1/include/global/dapps_list.h
phase_rev2et.o: /home/aps/r2.1.1/include/global/llist_err.h
phase_rev2et.o: /home/aps/r2.1.1/include/global/llist.h
phase_rev2et.o: /home/aps/r2.1.1/include/global/ord_llist.h
phase_rev2et.o: /home/aps/r2.1.1/include/local/db_phase.h
phase_rev2et.o: /home/aps/r2.1.1/include/global/dapps_defs.h
phase_rev2et.o: /home/aps/r2.1.1/include/local/timeconv.h
rev2asftime.o: /home/aps/r2.1.1/include/local/phase_utilities.h
rev2asftime.o: /home/aps/r2.1.1/include/global/db_sybint.h
rev2asftime.o: /home/sybase/sol/include/sybfront.h
rev2asftime.o: /home/sybase/sol/include/cstypes.h
rev2asftime.o: /home/sybase/sol/include/csconfig.h
rev2asftime.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
rev2asftime.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
rev2asftime.o: /usr/include/sys/feature_tests.h
rev2asftime.o: /home/sybase/sol/include/syblogin.h
rev2asftime.o: /home/sybase/sol/include/syberror.h
rev2asftime.o: /home/aps/r2.1.1/include/global/dapps_list.h
rev2asftime.o: /home/aps/r2.1.1/include/global/llist_err.h
rev2asftime.o: /home/aps/r2.1.1/include/global/llist.h
rev2asftime.o: /home/aps/r2.1.1/include/global/ord_llist.h
rev2asftime.o: /home/aps/r2.1.1/include/local/db_phase.h
rev2asftime.o: /home/aps/r2.1.1/include/global/dapps_defs.h
rev2asftime.o: /home/aps/r2.1.1/include/local/timeconv.h
rev_2_phase.o: /home/aps/r2.1.1/include/global/db_sybint.h
rev_2_phase.o: /home/sybase/sol/include/sybfront.h
rev_2_phase.o: /home/sybase/sol/include/cstypes.h
rev_2_phase.o: /home/sybase/sol/include/csconfig.h
rev_2_phase.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
rev_2_phase.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
rev_2_phase.o: /usr/include/sys/feature_tests.h
rev_2_phase.o: /home/sybase/sol/include/syblogin.h
rev_2_phase.o: /home/sybase/sol/include/syberror.h
rev_2_phase.o: /home/aps/r2.1.1/include/global/dapps_list.h
rev_2_phase.o: /home/aps/r2.1.1/include/global/llist_err.h
rev_2_phase.o: /home/aps/r2.1.1/include/global/llist.h
rev_2_phase.o: /home/aps/r2.1.1/include/global/ord_llist.h
rev_2_phase.o: /home/aps/r2.1.1/include/local/aps_db_table.h
rev_2_phase.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
rev_2_phase.o: /home/aps/r2.1.1/include/global/dapps_defs.h
rev_2_phase.o: /home/aps/r2.1.1/include/local/phase_utilities.h
rev_2_phase.o: /home/aps/r2.1.1/include/local/db_phase.h
rev_2_phase.o: /home/aps/r2.1.1/include/local/timeconv.h
