###############################################################################
#
# File:         lib_multiUser/Makefile 
#
# Function:     APS Makefile for libmu.a
#
# NOTE:  there is a TEST DRIVER for this library.  You do this:
#        make Testmu
#        and then run this executable.  There is some help offered there.
#
# Author:       L Stevens (from Ron Green (Inspired by S. Hardman))
#
# Date:         02/02/95
#
# SCCS Info:
#	"@(#)Makefile	5.1 98/01/08 APS/ASF"
#	"@(#) /home/aps/r2.1.2/src/lib_multiUser/SCCS/s.Makefile"
#
###############################################################################

ifdef CM_HOME
	APS_HOME = $(CM_HOME)/aps
endif

include ${APS_HOME}/src/aps_makedefs


###############################################################################
#
# Source Default Directory
#   Typically off of $APS_HOME/src... this is used to retrieve source
#
###############################################################################
 
SRC_DIR = $(APS_HOME)/src/lib_multiUser

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

#  When linking the Test Driver, use the very latest 
#  version of libmu.a, in the CURRENT directory.  This way, 
#  one can test the libmu.a library BEFORE delivering it:  
LALL =      -L.    \
			$(LAPS) \
			$(LSYBASE) \
			$(OSLFLAGS) \
			$($(OS)LFLAGS)

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

LIBS = $(EVERY_LIB)

###############################################################################
#
# Flags and Defines
#
###############################################################################

DEFS =      -D$(MACH)
#
#  -DPRINT_MU_INSERT flag prints the db record about to be inserted.
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

#
# Note:  As a precaution, the test driver is always DELETED when the
#        library is updated, to prevent anyone from using the driver on 
#        an obsolete version of the library.
#
#        The test driver is not made automatically, with 'make'. 
#        The CM people will not encounter the test driver when 
#        doing their configuration control procedures.
#
#       When linking the Test Driver, it uses the very latest 
#       version of libmu.a in this directory.  This way, 
#       one can test the libmu.a library BEFORE delivering it:  
#
TEST_DRIVER_EXE   = Testmulib
TEST_DRIVER_OBJ   = main.o

LIB          =  libmu.a

SRC          =  mu_dar_activity_request.c    \
                mu_error_message.c           \
                mu_get_new_permission_id.c   \
                mu_get_permission.c   \
                mu_get_unix_userid.c          \
                mu_insert_dar_perm.c         \
                mu_insert_planning_perm.c       \
                mu_insert_single_perm.c       \
                mu_pad_time_bracket.c           \
                mu_permission_request.c       \
                mu_permission_retrieve.c      \
                mu_permission_terminate.c     \
                mu_permission_validate.c      \
                mu_planning_activity_request.c       \
                mu_single_activity_request.c  \
                mu_sysprocesses.c              \
                mu_validate_activity_id.c     \
                mu_validate_sat_phase_nominal_orb.c     \
                mu_validate_station_id.c     \
                mu_verify_dar_perms.c        \
                mu_verify_planning_perms.c        \
                mu_verify_single_perms.c

HEADER_FILES =  mu.h

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

$(TEST_DRIVER_EXE):  $(TEST_DRIVER_OBJ)
	@echo
	@echo "Linking    $@"
	@$(CC) -o $@ $(TEST_DRIVER_OBJ) $(LALL) $(RALL) $(LIBS)
	@echo "Done"

$(EXE): $(OBJS)
	@echo
	@echo "Linking    $@"
	@$(CC) -o $@ $(OBJS) $(LALL) $(RALL) $(LIBS)
	@echo "Done"
 
$(LIB): $(OBJS)
	@echo
	@echo "Creating/Modifying Library $(LIB)"
	@$(ARCHIVE) rucv $@ $?
	@if [ $(OS) = sunos ]; then ranlib $(LIB); fi
	@echo "Done"
	@rm -f $(TEST_DRIVER_EXE)
# Note:  As a precaution, the test driver is always DELETED when the
#        library is updated, to prevent anyone from using the driver on 
#        an obsolete version of the library.

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

lint_Testmulib:
	$(LINT) $(IALL) main.c


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

mu_dar_activity_request.o: /usr/include/string.h
mu_dar_activity_request.o: /usr/include/sys/feature_tests.h
mu_dar_activity_request.o: /usr/include/stdio.h mu.h
mu_dar_activity_request.o: /home/aps/r2.1.1/include/global/db_sybint.h
mu_dar_activity_request.o: /home/sybase/sol/include/sybfront.h
mu_dar_activity_request.o: /home/sybase/sol/include/cstypes.h
mu_dar_activity_request.o: /home/sybase/sol/include/csconfig.h
mu_dar_activity_request.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
mu_dar_activity_request.o: /home/sybase/sol/include/sybdb.h
mu_dar_activity_request.o: /home/sybase/sol/include/syblogin.h
mu_dar_activity_request.o: /home/sybase/sol/include/syberror.h
mu_dar_activity_request.o: /home/aps/r2.1.1/include/global/dapps_list.h
mu_dar_activity_request.o: /home/aps/r2.1.1/include/global/llist_err.h
mu_dar_activity_request.o: /home/aps/r2.1.1/include/global/llist.h
mu_dar_activity_request.o: /home/aps/r2.1.1/include/global/ord_llist.h
mu_dar_activity_request.o: /home/aps/r2.1.1/include/global/dapps_defs.h
mu_dar_activity_request.o: /home/aps/r2.1.1/include/local/aps_db_table.h
mu_dar_activity_request.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
mu_dar_activity_request.o: /home/aps/r2.1.1/include/local/mu_utilities.h
mu_dar_activity_request.o: /home/aps/r2.1.1/include/local/db_active_dar_activities.h
mu_dar_activity_request.o: /home/aps/r2.1.1/include/local/db_permission_counter.h
mu_get_new_permission_id.o: mu.h /usr/include/string.h
mu_get_new_permission_id.o: /usr/include/sys/feature_tests.h
mu_get_new_permission_id.o: /usr/include/stdio.h
mu_get_new_permission_id.o: /home/aps/r2.1.1/include/global/db_sybint.h
mu_get_new_permission_id.o: /home/sybase/sol/include/sybfront.h
mu_get_new_permission_id.o: /home/sybase/sol/include/cstypes.h
mu_get_new_permission_id.o: /home/sybase/sol/include/csconfig.h
mu_get_new_permission_id.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
mu_get_new_permission_id.o: /home/sybase/sol/include/sybdb.h
mu_get_new_permission_id.o: /home/sybase/sol/include/syblogin.h
mu_get_new_permission_id.o: /home/sybase/sol/include/syberror.h
mu_get_new_permission_id.o: /home/aps/r2.1.1/include/global/dapps_list.h
mu_get_new_permission_id.o: /home/aps/r2.1.1/include/global/llist_err.h
mu_get_new_permission_id.o: /home/aps/r2.1.1/include/global/llist.h
mu_get_new_permission_id.o: /home/aps/r2.1.1/include/global/ord_llist.h
mu_get_new_permission_id.o: /home/aps/r2.1.1/include/global/dapps_defs.h
mu_get_new_permission_id.o: /home/aps/r2.1.1/include/local/aps_db_table.h
mu_get_new_permission_id.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
mu_get_new_permission_id.o: /home/aps/r2.1.1/include/local/mu_utilities.h
mu_get_new_permission_id.o: /home/aps/r2.1.1/include/local/db_permission_counter.h
mu_get_permission.o: /usr/include/stdlib.h /usr/include/sys/feature_tests.h
mu_get_permission.o: /usr/include/stdio.h /usr/include/string.h
mu_get_permission.o: /usr/include/unistd.h /usr/include/sys/types.h
mu_get_permission.o: /usr/include/sys/isa_defs.h /usr/include/sys/machtypes.h
mu_get_permission.o: /usr/include/sys/unistd.h
mu_get_permission.o: /home/aps/r2.1.1/include/local/aps_defs.h
mu_get_permission.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
mu_get_permission.o: /usr/include/syslog.h /usr/include/sys/syslog.h
mu_get_permission.o: /home/aps/r2.1.1/include/global/db_sybint.h
mu_get_permission.o: /home/sybase/sol/include/sybfront.h
mu_get_permission.o: /home/sybase/sol/include/cstypes.h
mu_get_permission.o: /home/sybase/sol/include/csconfig.h
mu_get_permission.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
mu_get_permission.o: /home/sybase/sol/include/sybdb.h
mu_get_permission.o: /home/sybase/sol/include/syblogin.h
mu_get_permission.o: /home/sybase/sol/include/syberror.h
mu_get_permission.o: /home/aps/r2.1.1/include/global/dapps_list.h
mu_get_permission.o: /home/aps/r2.1.1/include/global/llist_err.h
mu_get_permission.o: /home/aps/r2.1.1/include/global/llist.h
mu_get_permission.o: /home/aps/r2.1.1/include/global/ord_llist.h
mu_get_permission.o: /home/aps/r2.1.1/include/local/aps_db_table.h
mu_get_permission.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
mu_get_permission.o: /home/aps/r2.1.1/include/local/db_active_dar_activities.h
mu_get_permission.o: /home/aps/r2.1.1/include/local/db_active_single_activities.h
mu_get_permission.o: /home/aps/r2.1.1/include/local/db_active_planning_activities.h
mu_get_permission.o: /home/aps/r2.1.1/include/local/mu_utilities.h
mu_get_unix_userid.o: mu.h /usr/include/string.h
mu_get_unix_userid.o: /usr/include/sys/feature_tests.h /usr/include/stdio.h
mu_get_unix_userid.o: /home/aps/r2.1.1/include/global/db_sybint.h
mu_get_unix_userid.o: /home/sybase/sol/include/sybfront.h
mu_get_unix_userid.o: /home/sybase/sol/include/cstypes.h
mu_get_unix_userid.o: /home/sybase/sol/include/csconfig.h
mu_get_unix_userid.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
mu_get_unix_userid.o: /home/sybase/sol/include/sybdb.h
mu_get_unix_userid.o: /home/sybase/sol/include/syblogin.h
mu_get_unix_userid.o: /home/sybase/sol/include/syberror.h
mu_get_unix_userid.o: /home/aps/r2.1.1/include/global/dapps_list.h
mu_get_unix_userid.o: /home/aps/r2.1.1/include/global/llist_err.h
mu_get_unix_userid.o: /home/aps/r2.1.1/include/global/llist.h
mu_get_unix_userid.o: /home/aps/r2.1.1/include/global/ord_llist.h
mu_get_unix_userid.o: /home/aps/r2.1.1/include/global/dapps_defs.h
mu_get_unix_userid.o: /home/aps/r2.1.1/include/local/aps_db_table.h
mu_get_unix_userid.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
mu_get_unix_userid.o: /home/aps/r2.1.1/include/local/mu_utilities.h
mu_get_unix_userid.o: /usr/include/pwd.h /usr/include/sys/types.h
mu_get_unix_userid.o: /usr/include/sys/isa_defs.h
mu_get_unix_userid.o: /usr/include/sys/machtypes.h /usr/include/unistd.h
mu_get_unix_userid.o: /usr/include/sys/unistd.h
mu_insert_dar_perm.o: mu.h /usr/include/string.h
mu_insert_dar_perm.o: /usr/include/sys/feature_tests.h /usr/include/stdio.h
mu_insert_dar_perm.o: /home/aps/r2.1.1/include/global/db_sybint.h
mu_insert_dar_perm.o: /home/sybase/sol/include/sybfront.h
mu_insert_dar_perm.o: /home/sybase/sol/include/cstypes.h
mu_insert_dar_perm.o: /home/sybase/sol/include/csconfig.h
mu_insert_dar_perm.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
mu_insert_dar_perm.o: /home/sybase/sol/include/sybdb.h
mu_insert_dar_perm.o: /home/sybase/sol/include/syblogin.h
mu_insert_dar_perm.o: /home/sybase/sol/include/syberror.h
mu_insert_dar_perm.o: /home/aps/r2.1.1/include/global/dapps_list.h
mu_insert_dar_perm.o: /home/aps/r2.1.1/include/global/llist_err.h
mu_insert_dar_perm.o: /home/aps/r2.1.1/include/global/llist.h
mu_insert_dar_perm.o: /home/aps/r2.1.1/include/global/ord_llist.h
mu_insert_dar_perm.o: /home/aps/r2.1.1/include/global/dapps_defs.h
mu_insert_dar_perm.o: /home/aps/r2.1.1/include/local/aps_db_table.h
mu_insert_dar_perm.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
mu_insert_dar_perm.o: /home/aps/r2.1.1/include/local/mu_utilities.h
mu_insert_dar_perm.o: /home/aps/r2.1.1/include/local/db_active_dar_activities.h
mu_insert_planning_perm.o: mu.h /usr/include/string.h
mu_insert_planning_perm.o: /usr/include/sys/feature_tests.h
mu_insert_planning_perm.o: /usr/include/stdio.h
mu_insert_planning_perm.o: /home/aps/r2.1.1/include/global/db_sybint.h
mu_insert_planning_perm.o: /home/sybase/sol/include/sybfront.h
mu_insert_planning_perm.o: /home/sybase/sol/include/cstypes.h
mu_insert_planning_perm.o: /home/sybase/sol/include/csconfig.h
mu_insert_planning_perm.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
mu_insert_planning_perm.o: /home/sybase/sol/include/sybdb.h
mu_insert_planning_perm.o: /home/sybase/sol/include/syblogin.h
mu_insert_planning_perm.o: /home/sybase/sol/include/syberror.h
mu_insert_planning_perm.o: /home/aps/r2.1.1/include/global/dapps_list.h
mu_insert_planning_perm.o: /home/aps/r2.1.1/include/global/llist_err.h
mu_insert_planning_perm.o: /home/aps/r2.1.1/include/global/llist.h
mu_insert_planning_perm.o: /home/aps/r2.1.1/include/global/ord_llist.h
mu_insert_planning_perm.o: /home/aps/r2.1.1/include/global/dapps_defs.h
mu_insert_planning_perm.o: /home/aps/r2.1.1/include/local/aps_db_table.h
mu_insert_planning_perm.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
mu_insert_planning_perm.o: /home/aps/r2.1.1/include/local/mu_utilities.h
mu_insert_planning_perm.o: /home/aps/r2.1.1/include/local/db_active_planning_activities.h
mu_insert_single_perm.o: mu.h /usr/include/string.h
mu_insert_single_perm.o: /usr/include/sys/feature_tests.h
mu_insert_single_perm.o: /usr/include/stdio.h
mu_insert_single_perm.o: /home/aps/r2.1.1/include/global/db_sybint.h
mu_insert_single_perm.o: /home/sybase/sol/include/sybfront.h
mu_insert_single_perm.o: /home/sybase/sol/include/cstypes.h
mu_insert_single_perm.o: /home/sybase/sol/include/csconfig.h
mu_insert_single_perm.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
mu_insert_single_perm.o: /home/sybase/sol/include/sybdb.h
mu_insert_single_perm.o: /home/sybase/sol/include/syblogin.h
mu_insert_single_perm.o: /home/sybase/sol/include/syberror.h
mu_insert_single_perm.o: /home/aps/r2.1.1/include/global/dapps_list.h
mu_insert_single_perm.o: /home/aps/r2.1.1/include/global/llist_err.h
mu_insert_single_perm.o: /home/aps/r2.1.1/include/global/llist.h
mu_insert_single_perm.o: /home/aps/r2.1.1/include/global/ord_llist.h
mu_insert_single_perm.o: /home/aps/r2.1.1/include/global/dapps_defs.h
mu_insert_single_perm.o: /home/aps/r2.1.1/include/local/aps_db_table.h
mu_insert_single_perm.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
mu_insert_single_perm.o: /home/aps/r2.1.1/include/local/mu_utilities.h
mu_insert_single_perm.o: /home/aps/r2.1.1/include/local/db_active_single_activities.h
mu_pad_time_bracket.o: mu.h /usr/include/string.h
mu_pad_time_bracket.o: /usr/include/sys/feature_tests.h /usr/include/stdio.h
mu_pad_time_bracket.o: /home/aps/r2.1.1/include/global/db_sybint.h
mu_pad_time_bracket.o: /home/sybase/sol/include/sybfront.h
mu_pad_time_bracket.o: /home/sybase/sol/include/cstypes.h
mu_pad_time_bracket.o: /home/sybase/sol/include/csconfig.h
mu_pad_time_bracket.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
mu_pad_time_bracket.o: /home/sybase/sol/include/sybdb.h
mu_pad_time_bracket.o: /home/sybase/sol/include/syblogin.h
mu_pad_time_bracket.o: /home/sybase/sol/include/syberror.h
mu_pad_time_bracket.o: /home/aps/r2.1.1/include/global/dapps_list.h
mu_pad_time_bracket.o: /home/aps/r2.1.1/include/global/llist_err.h
mu_pad_time_bracket.o: /home/aps/r2.1.1/include/global/llist.h
mu_pad_time_bracket.o: /home/aps/r2.1.1/include/global/ord_llist.h
mu_pad_time_bracket.o: /home/aps/r2.1.1/include/global/dapps_defs.h
mu_pad_time_bracket.o: /home/aps/r2.1.1/include/local/aps_db_table.h
mu_pad_time_bracket.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
mu_pad_time_bracket.o: /home/aps/r2.1.1/include/local/mu_utilities.h
mu_pad_time_bracket.o: /home/aps/r2.1.1/include/local/db_phase.h
mu_pad_time_bracket.o: /home/aps/r2.1.1/include/local/timeconv.h
mu_permission_request.o: /usr/include/string.h
mu_permission_request.o: /usr/include/sys/feature_tests.h mu.h
mu_permission_request.o: /usr/include/stdio.h
mu_permission_request.o: /home/aps/r2.1.1/include/global/db_sybint.h
mu_permission_request.o: /home/sybase/sol/include/sybfront.h
mu_permission_request.o: /home/sybase/sol/include/cstypes.h
mu_permission_request.o: /home/sybase/sol/include/csconfig.h
mu_permission_request.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
mu_permission_request.o: /home/sybase/sol/include/sybdb.h
mu_permission_request.o: /home/sybase/sol/include/syblogin.h
mu_permission_request.o: /home/sybase/sol/include/syberror.h
mu_permission_request.o: /home/aps/r2.1.1/include/global/dapps_list.h
mu_permission_request.o: /home/aps/r2.1.1/include/global/llist_err.h
mu_permission_request.o: /home/aps/r2.1.1/include/global/llist.h
mu_permission_request.o: /home/aps/r2.1.1/include/global/ord_llist.h
mu_permission_request.o: /home/aps/r2.1.1/include/global/dapps_defs.h
mu_permission_request.o: /home/aps/r2.1.1/include/local/aps_db_table.h
mu_permission_request.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
mu_permission_request.o: /home/aps/r2.1.1/include/local/mu_utilities.h
mu_permission_request.o: /home/aps/r2.1.1/include/local/db_multi_user_wait.h
mu_permission_request.o: /home/aps/r2.1.1/include/local/db_active_dar_activities.h
mu_permission_request.o: /home/aps/r2.1.1/include/local/db_active_single_activities.h
mu_permission_request.o: /home/aps/r2.1.1/include/local/db_active_planning_activities.h
mu_permission_request.o: /home/aps/r2.1.1/include/local/db_permission_counter.h
mu_permission_retrieve.o: /usr/include/string.h
mu_permission_retrieve.o: /usr/include/sys/feature_tests.h mu.h
mu_permission_retrieve.o: /usr/include/stdio.h
mu_permission_retrieve.o: /home/aps/r2.1.1/include/global/db_sybint.h
mu_permission_retrieve.o: /home/sybase/sol/include/sybfront.h
mu_permission_retrieve.o: /home/sybase/sol/include/cstypes.h
mu_permission_retrieve.o: /home/sybase/sol/include/csconfig.h
mu_permission_retrieve.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
mu_permission_retrieve.o: /home/sybase/sol/include/sybdb.h
mu_permission_retrieve.o: /home/sybase/sol/include/syblogin.h
mu_permission_retrieve.o: /home/sybase/sol/include/syberror.h
mu_permission_retrieve.o: /home/aps/r2.1.1/include/global/dapps_list.h
mu_permission_retrieve.o: /home/aps/r2.1.1/include/global/llist_err.h
mu_permission_retrieve.o: /home/aps/r2.1.1/include/global/llist.h
mu_permission_retrieve.o: /home/aps/r2.1.1/include/global/ord_llist.h
mu_permission_retrieve.o: /home/aps/r2.1.1/include/global/dapps_defs.h
mu_permission_retrieve.o: /home/aps/r2.1.1/include/local/aps_db_table.h
mu_permission_retrieve.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
mu_permission_retrieve.o: /home/aps/r2.1.1/include/local/mu_utilities.h
mu_permission_retrieve.o: /home/aps/r2.1.1/include/local/db_multi_user_wait.h
mu_permission_terminate.o: /usr/include/string.h
mu_permission_terminate.o: /usr/include/sys/feature_tests.h mu.h
mu_permission_terminate.o: /usr/include/stdio.h
mu_permission_terminate.o: /home/aps/r2.1.1/include/global/db_sybint.h
mu_permission_terminate.o: /home/sybase/sol/include/sybfront.h
mu_permission_terminate.o: /home/sybase/sol/include/cstypes.h
mu_permission_terminate.o: /home/sybase/sol/include/csconfig.h
mu_permission_terminate.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
mu_permission_terminate.o: /home/sybase/sol/include/sybdb.h
mu_permission_terminate.o: /home/sybase/sol/include/syblogin.h
mu_permission_terminate.o: /home/sybase/sol/include/syberror.h
mu_permission_terminate.o: /home/aps/r2.1.1/include/global/dapps_list.h
mu_permission_terminate.o: /home/aps/r2.1.1/include/global/llist_err.h
mu_permission_terminate.o: /home/aps/r2.1.1/include/global/llist.h
mu_permission_terminate.o: /home/aps/r2.1.1/include/global/ord_llist.h
mu_permission_terminate.o: /home/aps/r2.1.1/include/global/dapps_defs.h
mu_permission_terminate.o: /home/aps/r2.1.1/include/local/aps_db_table.h
mu_permission_terminate.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
mu_permission_terminate.o: /home/aps/r2.1.1/include/local/mu_utilities.h
mu_permission_terminate.o: /home/aps/r2.1.1/include/local/db_multi_user_wait.h
mu_permission_terminate.o: /home/aps/r2.1.1/include/local/db_active_dar_activities.h
mu_permission_terminate.o: /home/aps/r2.1.1/include/local/db_active_planning_activities.h
mu_permission_terminate.o: /home/aps/r2.1.1/include/local/db_active_single_activities.h
mu_permission_validate.o: /usr/include/string.h
mu_permission_validate.o: /usr/include/sys/feature_tests.h mu.h
mu_permission_validate.o: /usr/include/stdio.h
mu_permission_validate.o: /home/aps/r2.1.1/include/global/db_sybint.h
mu_permission_validate.o: /home/sybase/sol/include/sybfront.h
mu_permission_validate.o: /home/sybase/sol/include/cstypes.h
mu_permission_validate.o: /home/sybase/sol/include/csconfig.h
mu_permission_validate.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
mu_permission_validate.o: /home/sybase/sol/include/sybdb.h
mu_permission_validate.o: /home/sybase/sol/include/syblogin.h
mu_permission_validate.o: /home/sybase/sol/include/syberror.h
mu_permission_validate.o: /home/aps/r2.1.1/include/global/dapps_list.h
mu_permission_validate.o: /home/aps/r2.1.1/include/global/llist_err.h
mu_permission_validate.o: /home/aps/r2.1.1/include/global/llist.h
mu_permission_validate.o: /home/aps/r2.1.1/include/global/ord_llist.h
mu_permission_validate.o: /home/aps/r2.1.1/include/global/dapps_defs.h
mu_permission_validate.o: /home/aps/r2.1.1/include/local/aps_db_table.h
mu_permission_validate.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
mu_permission_validate.o: /home/aps/r2.1.1/include/local/mu_utilities.h
mu_permission_validate.o: /home/aps/r2.1.1/include/local/db_active_dar_activities.h
mu_permission_validate.o: /home/aps/r2.1.1/include/local/db_active_planning_activities.h
mu_permission_validate.o: /home/aps/r2.1.1/include/local/db_active_single_activities.h
mu_planning_activity_request.o: /usr/include/string.h
mu_planning_activity_request.o: /usr/include/sys/feature_tests.h
mu_planning_activity_request.o: /usr/include/stdio.h
mu_planning_activity_request.o: /home/aps/r2.1.1/include/local/timeconv.h
mu_planning_activity_request.o: mu.h
mu_planning_activity_request.o: /home/aps/r2.1.1/include/global/db_sybint.h
mu_planning_activity_request.o: /home/sybase/sol/include/sybfront.h
mu_planning_activity_request.o: /home/sybase/sol/include/cstypes.h
mu_planning_activity_request.o: /home/sybase/sol/include/csconfig.h
mu_planning_activity_request.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
mu_planning_activity_request.o: /home/sybase/sol/include/sybdb.h
mu_planning_activity_request.o: /home/sybase/sol/include/syblogin.h
mu_planning_activity_request.o: /home/sybase/sol/include/syberror.h
mu_planning_activity_request.o: /home/aps/r2.1.1/include/global/dapps_list.h
mu_planning_activity_request.o: /home/aps/r2.1.1/include/global/llist_err.h
mu_planning_activity_request.o: /home/aps/r2.1.1/include/global/llist.h
mu_planning_activity_request.o: /home/aps/r2.1.1/include/global/ord_llist.h
mu_planning_activity_request.o: /home/aps/r2.1.1/include/global/dapps_defs.h
mu_planning_activity_request.o: /home/aps/r2.1.1/include/local/aps_db_table.h
mu_planning_activity_request.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
mu_planning_activity_request.o: /home/aps/r2.1.1/include/local/mu_utilities.h
mu_planning_activity_request.o: /home/aps/r2.1.1/include/local/db_active_planning_activities.h
mu_planning_activity_request.o: /home/aps/r2.1.1/include/local/db_permission_counter.h
mu_single_activity_request.o: /usr/include/string.h
mu_single_activity_request.o: /usr/include/sys/feature_tests.h
mu_single_activity_request.o: /usr/include/stdio.h mu.h
mu_single_activity_request.o: /home/aps/r2.1.1/include/global/db_sybint.h
mu_single_activity_request.o: /home/sybase/sol/include/sybfront.h
mu_single_activity_request.o: /home/sybase/sol/include/cstypes.h
mu_single_activity_request.o: /home/sybase/sol/include/csconfig.h
mu_single_activity_request.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
mu_single_activity_request.o: /home/sybase/sol/include/sybdb.h
mu_single_activity_request.o: /home/sybase/sol/include/syblogin.h
mu_single_activity_request.o: /home/sybase/sol/include/syberror.h
mu_single_activity_request.o: /home/aps/r2.1.1/include/global/dapps_list.h
mu_single_activity_request.o: /home/aps/r2.1.1/include/global/llist_err.h
mu_single_activity_request.o: /home/aps/r2.1.1/include/global/llist.h
mu_single_activity_request.o: /home/aps/r2.1.1/include/global/ord_llist.h
mu_single_activity_request.o: /home/aps/r2.1.1/include/global/dapps_defs.h
mu_single_activity_request.o: /home/aps/r2.1.1/include/local/aps_db_table.h
mu_single_activity_request.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
mu_single_activity_request.o: /home/aps/r2.1.1/include/local/mu_utilities.h
mu_single_activity_request.o: /home/aps/r2.1.1/include/local/db_active_single_activities.h
mu_single_activity_request.o: /home/aps/r2.1.1/include/local/db_permission_counter.h
mu_sysprocesses.o: /usr/include/string.h /usr/include/sys/feature_tests.h
mu_sysprocesses.o: mu.h /usr/include/stdio.h
mu_sysprocesses.o: /home/aps/r2.1.1/include/global/db_sybint.h
mu_sysprocesses.o: /home/sybase/sol/include/sybfront.h
mu_sysprocesses.o: /home/sybase/sol/include/cstypes.h
mu_sysprocesses.o: /home/sybase/sol/include/csconfig.h
mu_sysprocesses.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
mu_sysprocesses.o: /home/sybase/sol/include/sybdb.h
mu_sysprocesses.o: /home/sybase/sol/include/syblogin.h
mu_sysprocesses.o: /home/sybase/sol/include/syberror.h
mu_sysprocesses.o: /home/aps/r2.1.1/include/global/dapps_list.h
mu_sysprocesses.o: /home/aps/r2.1.1/include/global/llist_err.h
mu_sysprocesses.o: /home/aps/r2.1.1/include/global/llist.h
mu_sysprocesses.o: /home/aps/r2.1.1/include/global/ord_llist.h
mu_sysprocesses.o: /home/aps/r2.1.1/include/global/dapps_defs.h
mu_sysprocesses.o: /home/aps/r2.1.1/include/local/aps_db_table.h
mu_sysprocesses.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
mu_sysprocesses.o: /home/aps/r2.1.1/include/local/mu_utilities.h
mu_validate_activity_id.o: /usr/include/string.h
mu_validate_activity_id.o: /usr/include/sys/feature_tests.h
mu_validate_activity_id.o: /usr/include/libgen.h /usr/include/sys/types.h
mu_validate_activity_id.o: /usr/include/sys/isa_defs.h
mu_validate_activity_id.o: /usr/include/sys/machtypes.h /usr/include/stdio.h
mu_validate_activity_id.o: /usr/include/time.h mu.h
mu_validate_activity_id.o: /home/aps/r2.1.1/include/global/db_sybint.h
mu_validate_activity_id.o: /home/sybase/sol/include/sybfront.h
mu_validate_activity_id.o: /home/sybase/sol/include/cstypes.h
mu_validate_activity_id.o: /home/sybase/sol/include/csconfig.h
mu_validate_activity_id.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
mu_validate_activity_id.o: /home/sybase/sol/include/sybdb.h
mu_validate_activity_id.o: /home/sybase/sol/include/syblogin.h
mu_validate_activity_id.o: /home/sybase/sol/include/syberror.h
mu_validate_activity_id.o: /home/aps/r2.1.1/include/global/dapps_list.h
mu_validate_activity_id.o: /home/aps/r2.1.1/include/global/llist_err.h
mu_validate_activity_id.o: /home/aps/r2.1.1/include/global/llist.h
mu_validate_activity_id.o: /home/aps/r2.1.1/include/global/ord_llist.h
mu_validate_activity_id.o: /home/aps/r2.1.1/include/global/dapps_defs.h
mu_validate_activity_id.o: /home/aps/r2.1.1/include/local/aps_db_table.h
mu_validate_activity_id.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
mu_validate_activity_id.o: /home/aps/r2.1.1/include/local/mu_utilities.h
mu_validate_sat_phase_nominal_orb.o: /usr/include/libgen.h
mu_validate_sat_phase_nominal_orb.o: /usr/include/sys/types.h
mu_validate_sat_phase_nominal_orb.o: /usr/include/sys/feature_tests.h
mu_validate_sat_phase_nominal_orb.o: /usr/include/sys/isa_defs.h
mu_validate_sat_phase_nominal_orb.o: /usr/include/sys/machtypes.h
mu_validate_sat_phase_nominal_orb.o: /usr/include/stdio.h /usr/include/time.h
mu_validate_sat_phase_nominal_orb.o: mu.h /usr/include/string.h
mu_validate_sat_phase_nominal_orb.o: /home/aps/r2.1.1/include/global/db_sybint.h
mu_validate_sat_phase_nominal_orb.o: /home/sybase/sol/include/sybfront.h
mu_validate_sat_phase_nominal_orb.o: /home/sybase/sol/include/cstypes.h
mu_validate_sat_phase_nominal_orb.o: /home/sybase/sol/include/csconfig.h
mu_validate_sat_phase_nominal_orb.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
mu_validate_sat_phase_nominal_orb.o: /home/sybase/sol/include/sybdb.h
mu_validate_sat_phase_nominal_orb.o: /home/sybase/sol/include/syblogin.h
mu_validate_sat_phase_nominal_orb.o: /home/sybase/sol/include/syberror.h
mu_validate_sat_phase_nominal_orb.o: /home/aps/r2.1.1/include/global/dapps_list.h
mu_validate_sat_phase_nominal_orb.o: /home/aps/r2.1.1/include/global/llist_err.h
mu_validate_sat_phase_nominal_orb.o: /home/aps/r2.1.1/include/global/llist.h
mu_validate_sat_phase_nominal_orb.o: /home/aps/r2.1.1/include/global/ord_llist.h
mu_validate_sat_phase_nominal_orb.o: /home/aps/r2.1.1/include/global/dapps_defs.h
mu_validate_sat_phase_nominal_orb.o: /home/aps/r2.1.1/include/local/aps_db_table.h
mu_validate_sat_phase_nominal_orb.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
mu_validate_sat_phase_nominal_orb.o: /home/aps/r2.1.1/include/local/mu_utilities.h
mu_validate_sat_phase_nominal_orb.o: /home/aps/r2.1.1/include/local/db_phase.h
mu_validate_sat_phase_nominal_orb.o: /home/aps/r2.1.1/include/local/db_satsensor.h
mu_validate_station_id.o: /usr/include/stdio.h
mu_validate_station_id.o: /usr/include/sys/feature_tests.h mu.h
mu_validate_station_id.o: /usr/include/string.h
mu_validate_station_id.o: /home/aps/r2.1.1/include/global/db_sybint.h
mu_validate_station_id.o: /home/sybase/sol/include/sybfront.h
mu_validate_station_id.o: /home/sybase/sol/include/cstypes.h
mu_validate_station_id.o: /home/sybase/sol/include/csconfig.h
mu_validate_station_id.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
mu_validate_station_id.o: /home/sybase/sol/include/sybdb.h
mu_validate_station_id.o: /home/sybase/sol/include/syblogin.h
mu_validate_station_id.o: /home/sybase/sol/include/syberror.h
mu_validate_station_id.o: /home/aps/r2.1.1/include/global/dapps_list.h
mu_validate_station_id.o: /home/aps/r2.1.1/include/global/llist_err.h
mu_validate_station_id.o: /home/aps/r2.1.1/include/global/llist.h
mu_validate_station_id.o: /home/aps/r2.1.1/include/global/ord_llist.h
mu_validate_station_id.o: /home/aps/r2.1.1/include/global/dapps_defs.h
mu_validate_station_id.o: /home/aps/r2.1.1/include/local/aps_db_table.h
mu_validate_station_id.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
mu_validate_station_id.o: /home/aps/r2.1.1/include/local/mu_utilities.h
mu_validate_station_id.o: /home/aps/r2.1.1/include/local/db_station.h
mu_verify_dar_perms.o: mu.h /usr/include/string.h
mu_verify_dar_perms.o: /usr/include/sys/feature_tests.h /usr/include/stdio.h
mu_verify_dar_perms.o: /home/aps/r2.1.1/include/global/db_sybint.h
mu_verify_dar_perms.o: /home/sybase/sol/include/sybfront.h
mu_verify_dar_perms.o: /home/sybase/sol/include/cstypes.h
mu_verify_dar_perms.o: /home/sybase/sol/include/csconfig.h
mu_verify_dar_perms.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
mu_verify_dar_perms.o: /home/sybase/sol/include/sybdb.h
mu_verify_dar_perms.o: /home/sybase/sol/include/syblogin.h
mu_verify_dar_perms.o: /home/sybase/sol/include/syberror.h
mu_verify_dar_perms.o: /home/aps/r2.1.1/include/global/dapps_list.h
mu_verify_dar_perms.o: /home/aps/r2.1.1/include/global/llist_err.h
mu_verify_dar_perms.o: /home/aps/r2.1.1/include/global/llist.h
mu_verify_dar_perms.o: /home/aps/r2.1.1/include/global/ord_llist.h
mu_verify_dar_perms.o: /home/aps/r2.1.1/include/global/dapps_defs.h
mu_verify_dar_perms.o: /home/aps/r2.1.1/include/local/aps_db_table.h
mu_verify_dar_perms.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
mu_verify_dar_perms.o: /home/aps/r2.1.1/include/local/mu_utilities.h
mu_verify_dar_perms.o: /home/aps/r2.1.1/include/local/db_active_dar_activities.h
mu_verify_planning_perms.o: mu.h /usr/include/string.h
mu_verify_planning_perms.o: /usr/include/sys/feature_tests.h
mu_verify_planning_perms.o: /usr/include/stdio.h
mu_verify_planning_perms.o: /home/aps/r2.1.1/include/global/db_sybint.h
mu_verify_planning_perms.o: /home/sybase/sol/include/sybfront.h
mu_verify_planning_perms.o: /home/sybase/sol/include/cstypes.h
mu_verify_planning_perms.o: /home/sybase/sol/include/csconfig.h
mu_verify_planning_perms.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
mu_verify_planning_perms.o: /home/sybase/sol/include/sybdb.h
mu_verify_planning_perms.o: /home/sybase/sol/include/syblogin.h
mu_verify_planning_perms.o: /home/sybase/sol/include/syberror.h
mu_verify_planning_perms.o: /home/aps/r2.1.1/include/global/dapps_list.h
mu_verify_planning_perms.o: /home/aps/r2.1.1/include/global/llist_err.h
mu_verify_planning_perms.o: /home/aps/r2.1.1/include/global/llist.h
mu_verify_planning_perms.o: /home/aps/r2.1.1/include/global/ord_llist.h
mu_verify_planning_perms.o: /home/aps/r2.1.1/include/global/dapps_defs.h
mu_verify_planning_perms.o: /home/aps/r2.1.1/include/local/aps_db_table.h
mu_verify_planning_perms.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
mu_verify_planning_perms.o: /home/aps/r2.1.1/include/local/mu_utilities.h
mu_verify_planning_perms.o: /home/aps/r2.1.1/include/local/db_active_planning_activities.h
mu_verify_single_perms.o: mu.h /usr/include/string.h
mu_verify_single_perms.o: /usr/include/sys/feature_tests.h
mu_verify_single_perms.o: /usr/include/stdio.h
mu_verify_single_perms.o: /home/aps/r2.1.1/include/global/db_sybint.h
mu_verify_single_perms.o: /home/sybase/sol/include/sybfront.h
mu_verify_single_perms.o: /home/sybase/sol/include/cstypes.h
mu_verify_single_perms.o: /home/sybase/sol/include/csconfig.h
mu_verify_single_perms.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
mu_verify_single_perms.o: /home/sybase/sol/include/sybdb.h
mu_verify_single_perms.o: /home/sybase/sol/include/syblogin.h
mu_verify_single_perms.o: /home/sybase/sol/include/syberror.h
mu_verify_single_perms.o: /home/aps/r2.1.1/include/global/dapps_list.h
mu_verify_single_perms.o: /home/aps/r2.1.1/include/global/llist_err.h
mu_verify_single_perms.o: /home/aps/r2.1.1/include/global/llist.h
mu_verify_single_perms.o: /home/aps/r2.1.1/include/global/ord_llist.h
mu_verify_single_perms.o: /home/aps/r2.1.1/include/global/dapps_defs.h
mu_verify_single_perms.o: /home/aps/r2.1.1/include/local/aps_db_table.h
mu_verify_single_perms.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
mu_verify_single_perms.o: /home/aps/r2.1.1/include/local/mu_utilities.h
mu_verify_single_perms.o: /home/aps/r2.1.1/include/local/db_active_single_activities.h
