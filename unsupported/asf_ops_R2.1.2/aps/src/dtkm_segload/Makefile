###############################################################################
#
# File:         dtkm_segload/Makefile
#
# Function:     APS Makefile for aps_dtkm_segload
#
# Author:       Ron Green (Inspired by S. Hardman)
#
# Date:         02/02/95
#
# SCCS Info:
#	"@(#)Makefile	5.1 98/01/08 APS/ASF"
#	"@(#) /home/aps/r2.1.2/src/dtkm_segload/SCCS/s.Makefile"
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
 
SRC_DIR = $(APS_HOME)/src/dtkm_segload
 

###############################################################################
#
# Include Path Definitions
#    Delete lines that contain items you don't need
#    Always use the OSIFLAGS
#
###############################################################################

IALL =      $(IAPS) \
			$(ISYBASE) \
			$(OSIFLAGS) 

###############################################################################
#
# Library Path Definitions
#    Delete lines that contain items you don't need
#    Always use the OSLFLAGS
#
###############################################################################

LALL =      $(LAPS) \
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

# For siimplicity, use every library.  The linker will work it out...
LIBS = ${EVERY_LIB}

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

EXE          =  aps_dtkm_segload

LIB          =  

SRC          =  dtkm_segload.c     \
				dtkmseg_add_segment.c     \
				dtkm_segload_error_message.c     \
				dtkmseg_update_seg_dtkid.c

HEADER_FILES =  dtkm_segload.h

F77_SRC      =  

F77_INC      =  

OBJS         = $(SRC:%.c=%.o) $(F77_SRC:%.for=%.o)

TARGETS = $(EXE) $(LIB)
 

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

dtkm_segload.o: /usr/include/fcntl.h /usr/include/sys/types.h
dtkm_segload.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
dtkm_segload.o: /usr/include/sys/machtypes.h /usr/include/sys/fcntl.h
dtkm_segload.o: /usr/include/stdio.h /usr/include/string.h
dtkm_segload.o: /usr/include/stdlib.h /usr/include/sys/stat.h
dtkm_segload.o: /usr/include/sys/time.h /usr/include/sys/uio.h
dtkm_segload.o: /usr/include/unistd.h /usr/include/sys/unistd.h
dtkm_segload.o: /home/aps/r2.1.1/include/local/aps_log_msg.h
dtkm_segload.o: /usr/include/syslog.h /usr/include/sys/syslog.h
dtkm_segload.o: /home/aps/r2.1.1/include/local/aps_defs.h dtkm_segload.h
dtkm_segload.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkm_segload.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkm_segload.o: /home/sybase/sol/include/sybfront.h
dtkm_segload.o: /home/sybase/sol/include/cstypes.h
dtkm_segload.o: /home/sybase/sol/include/csconfig.h
dtkm_segload.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkm_segload.o: /home/sybase/sol/include/sybdb.h
dtkm_segload.o: /home/sybase/sol/include/syblogin.h
dtkm_segload.o: /home/sybase/sol/include/syberror.h
dtkm_segload.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkm_segload.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkm_segload.o: /home/aps/r2.1.1/include/global/llist.h
dtkm_segload.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkm_segload.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkm_segload.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkm_segload.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkm_segload.o: /home/aps/r2.1.1/include/local/db_seg.h
dtkm_segload.o: /home/aps/r2.1.1/include/local/db_maskinout.h
dtkm_segload.o: /home/aps/r2.1.1/include/local/db_station.h
dtkm_segload.o: /home/aps/r2.1.1/include/local/timeconv.h
dtkm_segload.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkm_segload.o: /home/aps/r2.1.1/include/local/mu_utilities.h
dtkmseg_add_segment.o: /usr/include/string.h /usr/include/sys/feature_tests.h
dtkmseg_add_segment.o: dtkm_segload.h
dtkmseg_add_segment.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkmseg_add_segment.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkmseg_add_segment.o: /home/sybase/sol/include/sybfront.h
dtkmseg_add_segment.o: /home/sybase/sol/include/cstypes.h
dtkmseg_add_segment.o: /home/sybase/sol/include/csconfig.h
dtkmseg_add_segment.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkmseg_add_segment.o: /home/sybase/sol/include/sybdb.h /usr/include/stdio.h
dtkmseg_add_segment.o: /home/sybase/sol/include/syblogin.h
dtkmseg_add_segment.o: /home/sybase/sol/include/syberror.h
dtkmseg_add_segment.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkmseg_add_segment.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkmseg_add_segment.o: /home/aps/r2.1.1/include/global/llist.h
dtkmseg_add_segment.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkmseg_add_segment.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkmseg_add_segment.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkmseg_add_segment.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkmseg_add_segment.o: /home/aps/r2.1.1/include/local/db_seg.h
dtkmseg_add_segment.o: /home/aps/r2.1.1/include/local/db_maskinout.h
dtkmseg_add_segment.o: /home/aps/r2.1.1/include/local/db_station.h
dtkmseg_add_segment.o: /home/aps/r2.1.1/include/local/timeconv.h
dtkmseg_add_segment.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkmseg_update_seg_dtkid.o: dtkm_segload.h
dtkmseg_update_seg_dtkid.o: /home/aps/r2.1.1/include/local/dtkm_utilities.h
dtkmseg_update_seg_dtkid.o: /home/aps/r2.1.1/include/global/db_sybint.h
dtkmseg_update_seg_dtkid.o: /home/sybase/sol/include/sybfront.h
dtkmseg_update_seg_dtkid.o: /home/sybase/sol/include/cstypes.h
dtkmseg_update_seg_dtkid.o: /home/sybase/sol/include/csconfig.h
dtkmseg_update_seg_dtkid.o: /home/aps/r2.1.1/src/makedepend_files/stsdef.h
dtkmseg_update_seg_dtkid.o: /home/sybase/sol/include/sybdb.h
dtkmseg_update_seg_dtkid.o: /usr/include/stdio.h
dtkmseg_update_seg_dtkid.o: /usr/include/sys/feature_tests.h
dtkmseg_update_seg_dtkid.o: /home/sybase/sol/include/syblogin.h
dtkmseg_update_seg_dtkid.o: /home/sybase/sol/include/syberror.h
dtkmseg_update_seg_dtkid.o: /home/aps/r2.1.1/include/global/dapps_list.h
dtkmseg_update_seg_dtkid.o: /home/aps/r2.1.1/include/global/llist_err.h
dtkmseg_update_seg_dtkid.o: /home/aps/r2.1.1/include/global/llist.h
dtkmseg_update_seg_dtkid.o: /home/aps/r2.1.1/include/global/ord_llist.h
dtkmseg_update_seg_dtkid.o: /home/aps/r2.1.1/include/local/aps_db_table.h
dtkmseg_update_seg_dtkid.o: /home/aps/r2.1.1/include/local/aps_db_tablenums.h
dtkmseg_update_seg_dtkid.o: /home/aps/r2.1.1/include/local/db_dtk.h
dtkmseg_update_seg_dtkid.o: /home/aps/r2.1.1/include/local/db_seg.h
dtkmseg_update_seg_dtkid.o: /home/aps/r2.1.1/include/local/db_maskinout.h
dtkmseg_update_seg_dtkid.o: /home/aps/r2.1.1/include/local/db_station.h
dtkmseg_update_seg_dtkid.o: /home/aps/r2.1.1/include/local/timeconv.h
dtkmseg_update_seg_dtkid.o: /home/aps/r2.1.1/include/global/dapps_defs.h
dtkmseg_update_seg_dtkid.o: /usr/include/string.h
